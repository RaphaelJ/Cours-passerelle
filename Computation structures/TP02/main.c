/* Computation Structures: Générateur de labyrinthes parallèle
 * Raphael Javaux - Novembre 2012.
 *
 * Génère et affiche un labyrinthe généré aléatoirement par un algorithme
 * concurrent.
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "labyrinth.h"

int main(void)
{
    srand(time(NULL));

    PARAL_STATS stats;
    LABYRINTH labyrinth = gen_labyrinth(&stats);

    show_labyrinth(labyrinth);

    free(labyrinth);

    printf(
        "Sur %d suppressions de murs, %d ont été effectuées en parallèle "
        "(%d %%).\n", stats.hits + stats.misses, stats.hits,
        (stats.hits * 100) / (stats.hits + stats.misses)
    );

    return 0;
}
