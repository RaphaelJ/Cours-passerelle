/* Computation Structures: Générateur de labyrinthes parallèle
 * Raphael Javaux - Novembre 2012.
 *
 * Ce fichier définit un algorithme pour générer des labyrinthes de 12 cases de
 * coté ainsi que la structure de données associée.
 */

#include <assert.h>
#include <string.h>

#include <signal.h>
#include <sys/msg.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <sys/wait.h>
#include <unistd.h>

#include "labyrinth.h"

const int LABYRINTH_SIZE = 12;

const CELL WALLS_MASK = 0xF000;
const CELL SHARE_MASK = 0x0800;
const CELL GROUP_MASK = 0x07FF;

// Chaque mur est associé à un masque lié à son bit.
const WALL WALL_TOP    = 0x8000;
const WALL WALL_RIGHT  = 0x4000;
const WALL WALL_BOTTOM = 0x2000;
const WALL WALL_LEFT   = 0x1000;

CELL *cell_index(LABYRINTH labyrinth, int x, int y)
{
    return labyrinth + y * LABYRINTH_SIZE + x;
}

bool is_wall(CELL cell, WALL wall_type)
{
    return cell & wall_type;
}

void close_wall(CELL *cell, WALL wall_type)
{
    *cell |= wall_type;
}

void open_wall(CELL *cell, WALL wall_type)
{
    *cell &= ~wall_type;
}

bool is_shared(CELL cell)
{
    return cell & SHARE_MASK;
}

void set_shared(CELL *cell)
{
    *cell |= SHARE_MASK;
}

CELL get_parent_index(CELL cell)
{
    return cell & GROUP_MASK;
}

void set_parent_index(CELL *cell, CELL parent_index)
{
    *cell &= ~GROUP_MASK;
    *cell |= parent_index;
}

CELL *cell_root(LABYRINTH labyrinth, CELL *cell)
{
    int parent_index = get_parent_index(*cell)
      , cell_index = cell - labyrinth;

    if (parent_index == cell_index) // La cellule est sa racine
        return cell;
    else {
        // Remonte l'arbre à la recherche de la racine (qui représente le
        // groupe de cellule).
        // Mets ensuite à jour la valeur de la cellule en cours pour qu'elle
        // pointe directement sur sa racine pour accélérer les parcours suivants
        // (la structure est ainsi gardée aplatie au fur et à mesure de
        // l'exécution de l'algorithme, ce qui garanti un temps algorithmique
        // quasiment constant pour tous les groupements de cellules qui seront
        // effectués. Voir article Union-Find de Wikipedia).
        CELL *root = cell_root(labyrinth, labyrinth + parent_index);
        int root_index = root - labyrinth;

        set_parent_index(cell, root_index);

        return root;
    }
}

void cell_attach_group(CELL *src, CELL dst)
{
    set_parent_index(src, get_parent_index(dst));
}

void init_labyrinth(LABYRINTH labyrinth)
{
    assert (
           LABYRINTH_SIZE % 2 == 0
        && LABYRINTH_SIZE * LABYRINTH_SIZE - 1 <= GROUP_MASK
    );

    // Chaque cellule se voit attribuer son propre groupe au début de la
    // génération du labyrinthe.
    for (int i = 0; i < LABYRINTH_SIZE * LABYRINTH_SIZE; i++)
        labyrinth[i] = WALLS_MASK | (CELL) i;

    // Spécifie que les cases limitrophes aux bordures des générateurs sont
    // partagées entre plusieurs processus.
    int max_index = LABYRINTH_SIZE - 1;
    int middle = max_index / 2;
    for (int i = 0; i < LABYRINTH_SIZE; i++) {
        for (int j = middle; j < middle + 2; j++) {
            set_shared(cell_index(labyrinth, i, j)); // Horizontale
            set_shared(cell_index(labyrinth, j, i)); // Verticale
        }
    }
}

// Décrémente la sémaphore de 1.
static void semwait(int sem);

// Incrémente la sémaphore de 1.
static void semsignal(int sem);

// Lance le généateur sur un autre processus.
static pid_t fork_generator(
    int sem_labyrinth, int shm_stats, int shm_labyrinth, int top_x, int top_y,
    int left_x, int left_y
);

// Génère une partie d'un labyrinthe. Les coordonnées correspondent aux
// positions des cellules du premier mur supérieur et du premier mur à gauche 
// de la zone du générateur.
static int generator(
    int sem_labyrinth, int shm_stats, int shm_labyrinth, int top_x, int top_y,
    int left_x, int left_y
);

LABYRINTH gen_labyrinth(PARAL_STATS *stats)
{
    // Crée une sémaphore qui sera utilisée comme mutex pour empêcher plusieurs
    // processus générateurs de supprimer en même temps une bordure entre deux
    // groupes de cellules qui sont partagés par plusieurs processus.
    // Tous les accès aux cellules partagées (limitrophes) ainsi qu'à l'ensemble
    // de leurs groupes respectifs devront se faire en ayant acquis cette mutex.
    int sem_labyrinth = semget(IPC_PRIVATE, 1, 0600);
    assert (sem_labyrinth != -1);
    semsignal(sem_labyrinth); // Initialise à 1 la sémaphore.

    // Ouvre une mémoire partagée pour stocker les statistiques de l'exécution
    // parallèle.
    int shm_stats = shmget(IPC_PRIVATE, sizeof (PARAL_STATS), 0600);
    assert (shm_stats != -1);
    PARAL_STATS *sh_stats = (PARAL_STATS *) shmat(shm_stats, NULL, 0);
    assert (sh_stats != (PARAL_STATS *) -1);
    sh_stats->hits = 0;
    sh_stats->misses = 0;

    // Crée une mémoire partagée pour stocker le labyrinthe.
    int shm_labyrinth = shmget(
        IPC_PRIVATE, sizeof (CELL) * LABYRINTH_SIZE * LABYRINTH_SIZE, 0600
    );
    assert (shm_labyrinth != -1);
    LABYRINTH labyrinth = (LABYRINTH) shmat(shm_labyrinth, NULL, 0);
    assert (labyrinth != (LABYRINTH) -1);

    init_labyrinth(labyrinth);

    // Démarre les quatre processus de génération (ne démarre que 3 processus
    // supplémentaires).
    int m = LABYRINTH_SIZE / 2;
    fork_generator(sem_labyrinth, shm_stats, shm_labyrinth, 0, 1, 1, 0);
    fork_generator(sem_labyrinth, shm_stats, shm_labyrinth, m, 1, m, 0);
    fork_generator(sem_labyrinth, shm_stats, shm_labyrinth, 0, m, 1, m);
    generator     (sem_labyrinth, shm_stats, shm_labyrinth, m, m, m, m);

    // Attend que toutes les cases des trois enfants soient toutes de la
    // même couleur.
    for (int i = 0; i < 3; i++) {
        int child_status;
        wait(&child_status);
        assert (child_status == EXIT_SUCCESS);
    }

    // Copie le résultat hors de la mémoire partagée.
    LABYRINTH ret_labyrinth = (LABYRINTH) malloc(
        sizeof (CELL) * LABYRINTH_SIZE * LABYRINTH_SIZE
    );
    memcpy(
        ret_labyrinth, labyrinth,
        sizeof (CELL) * LABYRINTH_SIZE * LABYRINTH_SIZE
    );

    // Récupère les statistiques.
    if (stats != NULL)
        *stats = *sh_stats;

    // Ferme les IPCs.
    assert (semctl(sem_labyrinth, 0, IPC_RMID) != -1);
    assert (shmctl(shm_stats, IPC_RMID, NULL) != -1);
    assert (shmctl(shm_labyrinth, IPC_RMID, NULL) != -1);

    return ret_labyrinth;
}

static void semwait(int sem)
{
    struct sembuf sbuf = { .sem_num = 0, .sem_op = -1, .sem_flg = 0 };
    assert (semop(sem, &sbuf, 1) != -1);
}

static void semsignal(int sem)
{
    struct sembuf sbuf = { .sem_num = 0, .sem_op = 1, .sem_flg = 0 };
    assert (semop(sem, &sbuf, 1) != -1);
}

static pid_t fork_generator(
    int sem_labyrinth, int shm_stats, int shm_labyrinth, int top_x, int top_y,
    int left_x, int left_y
)
{
    pid_t child = fork();
    if (child != 0) { // Parent
        assert (child != -1);
        return child;
    } else // Child
        exit(generator(
            sem_labyrinth, shm_stats, shm_labyrinth, top_x, top_y, 
            left_x, left_y)
        );
}

// Retourne la cellule qui partage le même mur.
static CELL *cell_fellow(CELL *cell, WALL orientation);

// Retourne l'orientation opposée d'un mur
static WALL orientation_inv(WALL orientation);

// Vérifie que certains murs privés ne partagent pas à présent les mêmes groupes
// ou qu'ils ne sont pas devenus partagés lorsque on a lié les deux groupes.
static void update_private(
    LABYRINTH labyrinth, CELL walls[], int *last_private, int *first_shared
);

static int generator(
    int sem_labyrinth, int shm_stats, int shm_labyrinth, int top_x, int top_y,
    int left_x, int left_y
)
{
    int size = LABYRINTH_SIZE / 2;

    // Récupère les mémoires partagées.
    PARAL_STATS *stats = (PARAL_STATS *) shmat(shm_stats, NULL, 0);
    assert (stats != (PARAL_STATS *) -1);
    LABYRINTH labyrinth = (LABYRINTH) shmat(shm_labyrinth, NULL, 0);
    assert (labyrinth != (LABYRINTH) -1);

    // Retiens les murs non ouverts dans la zone du générateur (indice de la
    // cellule avec orientation du mur, utilise les bits dédiés au groupe
    // pour encoder l'indice).
    // Ajoute les murs non partagés à partir de la gauche et ceux partagés à
    // partir de la droite du vecteur.
    int n_walls = 2 * size * size;
    int last_private = -1
      , first_shared = n_walls;
    CELL walls[n_walls];

    semwait(sem_labyrinth); // Accède en lecture à des cellules partagées.
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            int top_index  = (top_y + i)  * LABYRINTH_SIZE + top_x + j
              , left_index = (left_y + i) * LABYRINTH_SIZE + left_x + j;

            CELL *top_cell  = labyrinth + top_index
               , *left_cell = labyrinth + left_index;

            CELL top_fellow  = *cell_fellow(top_cell, WALL_TOP)
               , left_fellow = *cell_fellow(left_cell, WALL_LEFT);

            // Ajoute les murs sur des cases partagées à droite, les autres
            // à gauche.

            if (is_shared(*top_cell) && is_shared(top_fellow))
                walls[--first_shared] = WALL_TOP | (CELL) top_index;
            else
                walls[++last_private] = WALL_TOP | (CELL) top_index;

            if (is_shared(*left_cell) && is_shared(left_fellow))
                walls[--first_shared] = WALL_LEFT | (CELL) left_index;
            else
                walls[++last_private] = WALL_LEFT | (CELL) left_index;
        }
    }
    semsignal(sem_labyrinth);

    // Boucle tant qu'il reste des murs à supprimer.
    for (;;) {
        semwait(sem_labyrinth);

        // Vérifie que les murs partagés n'ont pas été supprimés par un autre
        // processus et qu'ils départagent toujours deux groupes différents.
        for (int i = n_walls - 1; i >= first_shared; i--) {
            CELL wall = walls[i];
            CELL index = wall & GROUP_MASK;
            WALL orientation = wall & WALLS_MASK;
            CELL *cell1 = labyrinth + index;
            CELL *cell2 = cell_fellow(cell1, orientation);

            bool can_be_removed =
                   is_wall(*cell1, orientation)
                && cell_root(labyrinth, cell1) != cell_root(labyrinth, cell2);

            if (!can_be_removed) // Ne peut plus supprimer ce mur.
                walls[i++] = walls[first_shared++];
        }

        // Sélectionne au hasard un mur dans les murs restants.
        int n_remaining = (last_private + 1) + (n_walls - first_shared);
        if (n_remaining == 0) {
            semsignal(sem_labyrinth);
            break;
        }
        int random_index = rand() % n_remaining;

        if (random_index <= last_private) {
            stats->hits++;
            semsignal(sem_labyrinth);

            // Cette partie de l'algorithme peut s'effectuer en même temps sur
            // différents processus :

            CELL wall = walls[random_index];
            CELL index = wall & GROUP_MASK;
            WALL orientation = wall & WALLS_MASK;
            CELL *cell1 = labyrinth + index;
            CELL *cell2 = cell_fellow(cell1, orientation);
            CELL *root1 = cell_root(labyrinth, cell1)
               , *root2 = cell_root(labyrinth, cell2);

            // Attache toujours une cellule qui n'appartient pas à un groupe
            // partagé à l'autre cellule. De cette manière, la racine d'un
            // groupe dont au moins une cellule est partagée sera une cellule
            // partagée. Ceci évite également de modifier une forêt qui pourrait
            // être modifiée par un autre processus au même moment.
            if (is_shared(*root1))
                cell_attach_group(root2, *root1);
            else
                cell_attach_group(root1, *root2);
            // Supprime le mur de la liste d'attente et des deux cellules.
            walls[random_index] = walls[last_private--];

            // Ces deux instructions sont équivalentes à ces deux instructions:
            // *cell1 &= ~orientation;
            // *cell2 &= ~orientation_inv(orientation);
            // A l'exception que le compilateur garantit que ces instructions
            // s'exécutent de manière atomique, et permet ainsi d'éviter que 
            // deux processus suppriment deux murs différents d'une même cellule
            // au même instant, ce qui engendrait une perte de l'une des deux
            // modifications.
            __sync_fetch_and_and(cell1, ~orientation);
            __sync_fetch_and_and(cell2, ~orientation_inv(orientation));
            // Une alternative aurait été d'acquérir la sémaphore sem_labyrinth
            // lorsque l'on change l'orientation d'une cellule qui est sur un
            // bord de la zone du générateur, mais cette méthode est
            // considérablement plus rapide.

            update_private(labyrinth, walls, &last_private, &first_shared);
        } else {
            stats->misses++;

            int wall_index = random_index - (last_private + 1) + first_shared;
            CELL wall = walls[wall_index];
            CELL index = wall & GROUP_MASK;
            WALL orientation = wall & WALLS_MASK;
            CELL *cell1 = labyrinth + index;
            CELL *cell2 = cell_fellow(cell1, orientation);
            CELL *root1 = cell_root(labyrinth, cell1);

            cell_attach_group(root1, *cell2);

            // Supprime le mur de la liste d'attente et des deux cellules.
            walls[wall_index] = walls[first_shared++];
            *cell1 &= ~orientation;
            *cell2 &= ~orientation_inv(orientation);

            semsignal(sem_labyrinth);
        }
    }
    return EXIT_SUCCESS;
}

static CELL *cell_fellow(CELL *cell, WALL orientation)
{
    // Switch-case non utilisable ici car orientation n'est pas un entier :-(.
    if (orientation == WALL_TOP)
        return cell - LABYRINTH_SIZE;
    else if (orientation == WALL_RIGHT)
        return cell + 1;
    else if (orientation == WALL_BOTTOM)
        return cell + LABYRINTH_SIZE;
    else // WALL_LEFT
        return cell - 1;
}

static WALL orientation_inv(WALL orientation)
{
    if (orientation == WALL_TOP)
        return WALL_BOTTOM;
    else if (orientation == WALL_RIGHT)
        return WALL_LEFT;
    else if (orientation == WALL_BOTTOM)
        return WALL_TOP;
    else  // WALL_LEFT
        return WALL_RIGHT;
}

static void update_private(
    LABYRINTH labyrinth, CELL walls[], int *last_private, int *first_shared
)
{
    for (int i = 0; i <= *last_private; i++) {
        CELL wall = walls[i];
        CELL index = wall & GROUP_MASK;
        WALL orientation = wall & WALLS_MASK;
        CELL *cell1 = labyrinth + index;
        CELL *cell2 = cell_fellow(cell1, orientation);

        CELL *root1 = cell_root(labyrinth, cell1)
           , *root2 = cell_root(labyrinth, cell2);

        if (root1 == root2) // Le mur ne peut plus être supprimé.
            walls[i--] = walls[(*last_private)--];
        else if (is_shared(*root1) && is_shared(*root2)) {
            // Déplace le mur dans les murs partagés.
            walls[--(*first_shared)] = walls[i];
            walls[i--] = walls[(*last_private)--];
        }
    }
}

void show_labyrinth(const LABYRINTH labyrinth)
{
    for (int y = 0; y < LABYRINTH_SIZE; y++) {
        // Dessine les bordures supérieures de la ligne.
        for (int x = 0; x < LABYRINTH_SIZE; x++) {
            if (is_wall(*cell_index(labyrinth, x, y), WALL_TOP))
                printf("·――");
            else
                printf("·  ");
        }
        printf("·\n");

        // Dessine les bordures latérales des cellules de la ligne
        for (int x = 0; x < LABYRINTH_SIZE; x++) {
            if (is_wall(*cell_index(labyrinth, x, y), WALL_LEFT))
                printf("|  ");
            else
                printf("   ");
        }
        if (is_wall(*cell_index(labyrinth, LABYRINTH_SIZE - 1, y), WALL_RIGHT))
            putchar('|');

        putchar('\n');
    }

    // Dessine la bordure inférieure de la dernière ligne
    for (int x = 0; x < LABYRINTH_SIZE; x++) {
        if (is_wall(*cell_index(labyrinth, x, LABYRINTH_SIZE - 1), WALL_BOTTOM))
            printf("·――");
        else
            printf("·  ");
    }
    printf("·\n");
}
