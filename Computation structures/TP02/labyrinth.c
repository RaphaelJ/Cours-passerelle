/* Computation Structures: Générateur de labyrinthes parallèle
 * Raphael Javaux - Novembre 2012.
 *
 * Ce fichier définit un algorithme pour générer des labyrinthes de 12 cases de
 * coté ainsi que la structure de données associée.
 */

#include <assert.h>
#include <string.h>

#include <signal.h>
#include <sys/shm.h>
#include <sys/msg.h>
#include <sys/wait.h>

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
        labyrinth[i] = (CELL) i;

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

    return labyrinth;
}

LABYRINTH gen_labyrinth(PARAL_STATS *stats)
{
    // Crée une sémaphore qui sera utilisée comme mutex pour empêcher plusieurs
    // processus générateurs de supprimer en même temps une bordure entre deux
    // groupes de cellules qui est sont accessibles par plusieurs processus.
    // Tous les accès aux cellules partagées (limitrophes) ainsi qu'à leur
    // groupes respectifs devront se faire en ayant acquis cette mutex.
    int sem_labyrinth = semget(IPC_PRIVATE, 1, 0600);
    assert (sem_labyrinth != -1);
    struct sembuf sbuf = { 0, 1, 0 };
    assert (semop(sem_labyrinth, &sbuf, 1) != -1);

    // Ouvre une mémoire partagée pour stocker les statistiques de l'exécution
    // parallèle.
    int shm_stats = shmget(IPC_PRIVATE, sizeof (PARAL_STATS), 0600);
    assert (shm_stats != -1);
    PARAL_STATS *sh_stats = (PARAL_STATS *) shmat(shm_stats, NULL, 0);
    assert (sh_stats != -1);

    // Crée une mémoire partagée pour stocker le labyrinthe.
    int shm_labyrinth = shmget(
        IPC_PRIVATE, sizeof (CELL) * LABYRINTH_SIZE * LABYRINTH_SIZE, 0600
    );
    assert (shm_labyrinth != -1);
    LABYRINTH labyrinth = (LABYRINTH) shmat(shm_labyrinth, NULL, 0);
    assert (labyrinth != -1);

    init_labyrinth(labyrinth);

    // Démarre les quatre processus de génération (ne démarre que 3 processus
    // supplémentaires).
    int middle = LABYRINTH_SIZE / 2;
    fork_generator(sem_labyrinth, shm_stats, shm_labyrinth, 1     , 1     );
    fork_generator(sem_labyrinth, shm_stats, shm_labyrinth, middle, 1     );
    fork_generator(sem_labyrinth, shm_stats, shm_labyrinth, middle, 1     );
    generator     (sem_labyrinth, shm_stats, shm_labyrinth, middle, middle);

    // Attends que toutes les cases des trois enfants soient toutes de la
    // même couleur.
    int childs_status;
    for (int i = 0; i < 3; i++)
        assert (wait(&childs_status) == EXIT_SUCCESS);

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

static pid_t fork_generator(
    int sem_labyrinth, int shm_stats, int shm_labyrinth, int x, int y
)
{
    pid_t child = fork();
    if (child != 0) { // Parent
        assert (child != -1);
        return child;
    } else // Child
        exit(generator(sem_labyrinth, shm_stats, shm_labyrinth, x, y));
}

static int generator(
    int sem_labyrinth, int shm_stats, int shm_labyrinth, int x, int y
)
{
    int size = LABYRINTH_SIZE / 2;

    // Récupère les mémoires partagées.
    LABYRINTH labyrinth = (LABYRINTH) shmat(shm_labyrinth, NULL, 0);
    assert (labyrinth != -1);
    PARAL_STATS *stats = (PARAL_STATS *) shmat(shm_stats, NULL, 0);
    assert (stats != -1);

    // Retiens les murs non ouverts dans la zone du générateur (indice de la
    // cellule avec orientation du mur, utilise les bits dédiés au groupe
    // pour encoder l'indice).
    // Ajoute les murs non partagés à partir de la gauche et ceux partagés à
    // partir de la droite du vecteur.
    int n_walls = 2 * middle * middle;
    int last_private = -1
      , last_shared = n_walls;
    CELL walls[n_walls];

    semwait(sem_labyrinth); // Accède en lecture à des cellules partagées.
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; i++) {
            int index = y * LABYRINTH_SIZE + x;
            CELL *cell = labyrinth + index;

            // Ajoute les murs sur des cases partagées à droite, les autres
            // à gauche.
            bool shared = is_shared(*cell)
               , left_shared = is_shared(*(cell - 1))
               , top_shared = is_shared(*(cell - LABYRINTH_SIZE));

            if (shared || left_shared)
                walls[--last_shared] = WALL_LEFT | (CELL) index;
            else
                walls[++last_private] = WALL_LEFT | (CELL) index;

            if (shared || top_shared)
                walls[--last_shared] = WALL_TOP | (CELL) index;
            else
                walls[++last_private] = WALL_TOP | (CELL) index;
        }
    }
    semsignal(sem_labyrinth);

    // Boucle tant qu'il reste des murs à supprimer.
    for  (;;) {
        semwait(sem_labyrinth);

        // Vérifie que les murs partagés n'ont pas été supprimés par un autre
        // processus et qu'ils départagent toujours deux groupes différents.
        for (int i = n_walls - 1; i >= last_shared; i--) {
            CELL wall = walls[i];
            CELL index = wall & GROUP_MASK;
            WALL orientation = wall & WALLS_MASK;
            CELL *cell = labyrinth + index;
            CELL *fellow = cell_fellow(labyrinth, cell, orientation);

            bool removable =
                   is_wall(cell, orientation)
                && cell_root(labyrinth, cell) != cell_root(labyrinth, fellow);

            if (!removable) // Mur supprimé
                walls[i] = walls[last_shared++];
        }

        // Sélectionne au hasard un mur dans les murs restants.
        int n_remaining = (last_private + 1) + (n_walls - last_shared);
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
            CELL *cell2 = cell_fellow(labyrinth, cell1, orientation);
            CELL *root1 = cell_root(labyrinth, cell1)
               , *root2 = cell_root(labyrinth, cell2);
            bool shared1 = is_shared(*root1)
               , shared2 = is_shared(*root2);

            // Attache toujours une cellule qui n'appartient pas à un groupe
            // partagé à l'autre cellule. De cette manière, la racine d'un
            // groupe dont au moins une cellule est partagée sera une cellule
            // partagée. Ceci évite également de modifier une forêt qui pourrait
            // être modifiée par un autre processus au même moment.
            if (shared1)
                cell_attach_group(root2, *root1);
            else
                cell_attach_group(root1, *root2);

            // Supprime le mur de la liste d'attente
            *(walls + random_index) = *(walls + last_private--);

            // Vérifie que certains murs privés ne sont pas devenus partagés
            // lorsque on a lié les deux groupes et que l'un de ces deux-ci
            // était partagé.
            if (shared1 || shared2) {
                for (int i = 0; i <= last_private; i++) {
                    
                }
            }
        } else {
            stats->miss++;

            semsignal(sem_labyrinth);
        }
    }

    return EXIT_SUCCESS;
}

// Retourne la cellule qui partage le même mur.
static CELL *cell_fellow(LABYRINTH labyrinth, CELL *cell, WALL orientation)
{
    switch (orientation) {
    case WALL_TOP:
        return cell - LABYRINTH_SIZE;
    case WALL_RIGHT:
        return cell + 1;
    case WALL_BOTTOM:
        return cell + LABYRINTH_SIZE;
    default: // WALL_LEFT
        return cell - 1;
    }
}

static int semwait()
{
    
}

static int semsignal()
{
    
}

static void cells_swap(CELL *c1, CELL *c2)
{
    CELL tmp = *c1;
    *c1 = *c2;
    *c2 = tmp;
}

void show_labyrinth(const LABYRINTH labyrinth)
{
    for (int y = 0; y < LABYRINTH_SIZE; y++) {
        // Dessine les bordures supérieures de la ligne.
        for (int x = 0; x < LABYRINTH_SIZE; x++) {
            if (is_wall(*cell_index(labyrinth, x, y), WALL_TOP))
                printf(" ――");
            else
                printf("   ");
        }
        putchar('\n');

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
            printf(" ――");
        else
            printf("   ");
    }
    putchar('\n');
}
