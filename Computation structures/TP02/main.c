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
    s_rand(time(NULL));

    LABYRINTH labyrinth = gen_labyrinth();

    show_labyrinth(labyrinth);

    free(labyrinth);

    return 0;
}
