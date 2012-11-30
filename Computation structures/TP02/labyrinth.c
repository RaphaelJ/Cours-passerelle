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

CELL cell_root(LABYRINTH labyrinth, CELL *cell)
{
    int parent_index = get_parent_index(*cell)
      , cell_index = cell - labyrinth;

    if (parent_index == cell_index) // La cellule est la racine
        return *cell;
    else {
        // Remonte l'arbre à la recherche de la racine (qui représente le
        // groupe de cellule).
        // Mets ensuite à jour la valeur de la cellule en cours pour qu'elle
        // pointe directement sur sa racine pour accélérer les parcours suivants
        // (la structure est ainsi gardée aplatie au fur et à mesure de
        // l'exécution de l'algorithme, ce qui garanti un temps algorithmique
        // quasiment constant pour tous les groupements de cellules qui seront
        // effectués. Voir article Union-Find de Wikipedia).
        CELL root = cell_root(labyrinth, labyrinth + parent_index);
        CELL root_index = root - labyrinth;

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
    assert (LABYRINTH_SIZE % 2 == 0);

    for (int i = 0; i < LABYRINTH_SIZE * LABYRINTH_SIZE; i++)
        labyrinth[i] = WALLS_MASK | i;

    // Spécifie que les cases limitrophes aux bordures des générateurs sont
    // partagées entre plusieurs processus.
    int max_index = LABYRINTH_SIZE - 1;
    int middle = max_index / 2;
    for (int i = 0; i < LABYRINTH_SIZE; i++) {
        for (int j = middle; j <= middle + max_index % 2; j++) {
            set_shared(labyrinth + i * LABYRINTH_SIZE + j); // Verticale
            set_shared(labyrinth + j * LABYRINTH_SIZE + i); // Horizontale
        }
    }

    return labyrinth;
}

LABYRINTH gen_labyrinth(PARAL_STATS *stats)
{
    // Crée une sémaphore qui sera utilisée comme mutex pour empêcher plusieurs
    // processus générateurs de supprimer en même temps une bordure entre deux
    // groupes de cellules qui est sont accessibles par plusieurs processus.
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
    LABYRINTH sh_labyrinth = (LABYRINTH) shmat(shm_labyrinth, NULL, 0);
    assert (sh_labyrinth != -1);

    init_labyrinth(sh_labyrinth);

    // Démarre les quatre processuss générateur.
    int middle = LABYRINTH_SIZE / 2;
    fork(sem_labyrinth, shm_stats, sh_labyrinth, 0     , 0     );
    fork(sem_labyrinth, shm_stats, sh_labyrinth, middle, 0     );
    fork(sem_labyrinth, shm_stats, sh_labyrinth, middle, 0     );
    fork(sem_labyrinth, shm_stats, sh_labyrinth, middle, middle);

    // Attends que toutes les cases des quatre processus soient toutes de la
    // même couleur.
    int childs_status;
    for (int i = 0; i < 4; i++)
        wait(&childs_status);

    // Copie le labyrinthe dans une mémoire non partagée
    LABYRINTH labyrinth = (LABYRINTH) malloc(
        sizeof (CELL) * LABYRINTH_SIZE * LABYRINTH_SIZE
    );
    memcpy(
        labyrinth, sh_labyrinth, sizeof (CELL) * LABYRINTH_SIZE * LABYRINTH_SIZE
    );

    // Récupère les statistiques.
    if (stats != NULL)
        *stats = *sh_stats;

    // Ferme les IPC
    assert (semctl(sem_labyrinth, 0, IPC_RMID) != -1);
    assert (shmctl(shm_stats, IPC_RMID, NULL) != -1);
    assert (shmctl(shm_labyrinth, IPC_RMID, NULL) != -1);

    return labyrinth;
}

static pid_t fork_generator(
    int sem_labyrinth, int shm_stats, int sh_labyrinth, int x, int y
)
{
    pid_t child = fork();
    if (child != 0) { // Parent
        assert (child != -1);
        return child;
    } else { // Child
        exit(generator(sem_labyrinth, shm_stats, sh_labyrinth, x, y));
    }
}

static int generator(
    int sem_labyrinth, int shm_stats, int sh_labyrinth, int x, int y
)
{
    int middle = LABYRINTH_SIZE / 2;
    int max_x = x + middle, max_y = y + middle;

    

    return EXIT_SUCCESS;
}

void show_labyrinth(const LABYRINTH labyrinth)
{
    for (int ln = 0; ln < LABYRINTH_SIZE; ln++) {
        const CELL *line = labyrinth + ln * LABYRINTH_SIZE;

        // Dessine les bordures supérieures de la ligne.
        for (int col = 0; col < LABYRINTH_SIZE; col++) {
            if (is_wall(line[col], WALL_TOP))
                printf(" ――");
            else
                printf("   ");
        }
        putchar('\n');

        // Dessine les bordures latérales des cellules de la ligne
        for (int col = 0; col < LABYRINTH_SIZE; col++) {
            if (is_wall(line[col], WALL_LEFT))
                printf("|  ");
            else
                printf("   ");
        }
        if (is_wall(line[LABYRINTH_SIZE - 1], WALL_RIGHT))
            putchar('|');

        putchar('\n');
    }

    // Dessine la bordure inférieure de la dernière ligne
    const CELL *last = labyrinth + LABYRINTH_SIZE * (LABYRINTH_SIZE - 1);
    for (int col = 0; col < LABYRINTH_SIZE; col++) {
        if (is_wall(last[col], WALL_BOTTOM))
            printf(" ――");
        else
            printf("   ");
    }
    putchar('\n');
}
