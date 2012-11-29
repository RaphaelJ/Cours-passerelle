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
    
    
    // Crée quatre files de messages pour les quatre processus générateurs.
    int queues_walls = msgget(IPC_PRIVATE, 4, 0600);
    assert (queues_walls != -1);

    // Crée quatre sémaphores qui vont permettre au processus principal
    // d'attendre qu'il n'y aie plus de murs à effacer sur les quatres processus
    // générateurs avant de mettre fin à ceux-ci.
    // Initialise les quatre sémaphores à 0. Elles seronts mises à 1 par chaque
    // processus lorsqu'il aura fini son travail d'ouverture de mur et qu'il
    // sera uniquement en train d'attendre des remplissages à effectuer pour
    // d'autres processus.
    int sem_gens = semget(IPC_PRIVATE, 4, 0600);
    assert (sem_gens != -1);
    struct sembuf sbuf[4] = {
        { 0, 1, 0 }, { 1, 1, 0 }, { 2, 1, 0 }, { 3, 1, 0 }
    };
    assert (semop(sem_gens, &sbuf, 4) != -1);

    // Crée une sémaphore qui sera utilisée comme mutex pour empêcher plusieurs
    // processus générateurs de supprimer en même temps une bordure entre deux
    // groupes de cellules qui est sont accessibles par plusieurs processus.
    int sem_labyrinth = semget(IPC_PRIVATE, 4, 0600);
    assert (sem_gens != -1);
    struct sembuf sbuf = { 0, 1, 0 };
    assert (semop(sem_gens, &sbuf, 4) != -1);
    

    // Ouvre une mémoire partagée pour stocker le labyrinthe.
    int shm_labyrinth = shmget(
        IPC_PRIVATE, sizeof (CELL) * LABYRINTH_SIZE * LABYRINTH_SIZE, 0600
    );
    assert (shm_labyrinth != -1);
    LABYRINTH sh_labyrinth = (LABYRINTH) shmat(shm_labyrinth, NULL, 0);
    assert (sh_labyrinth != -1);

    init_labyrinth(sh_labyrinth);

    // Attends que toutes les cases des quatre processus soient toutes de la
    // même couleur. Tue ensuite ces processus.
    sbuf = { { 0, -1, 0 }, { 1, -1, 0 }, { 2, -1, 0 }, { 3, -1, 0 } };
    assert (semop(sem_gens, &sbuf, 4) != -1);
    kill(pid[0], SIGTERM);
    kill(pid[1], SIGTERM);
    kill(pid[2], SIGTERM);
    kill(pid[3], SIGTERM);

    // Copie le labyrinthe dans une mémoire non partagée
    LABYRINTH labyrinth = (LABYRINTH) malloc(
        sizeof (CELL) * LABYRINTH_SIZE * LABYRINTH_SIZE
    );
    memcpy(
        labyrinth, sh_labyrinth, sizeof (CELL) * LABYRINTH_SIZE * LABYRINTH_SIZE
    );

    // Ferme les IPC
    assert (msgctl(queues_walls, IPC_RMID, NULL) != -1);
    assert (semctl(sem_gens, 0, IPC_RMID) != -1);
    assert (shmctl(shm_labyrinth, IPC_RMID, NULL) != -1);

    return labyrinth;
}

static pid_t fork_generator(int generator_id)
{
    pid_t child = fork();
    if (child != 0) { // Parent
        assert (child != -1);
        return child;
    } else { // Child
        
    }
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
