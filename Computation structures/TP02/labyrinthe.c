/* Computation Structures: Générateur de labyrinthes parallèle
 * Raphael Javaux - Novembre 2012.
 *
 * Ce fichier définit un algorithme pour générer des labyrinthes de 12 cases de
 * coté ainsi que la structure de données associée.
 */

#include <assert.h>

#include "labyrinthe.h"

uint32_t cell_color(CELL cell)
{
    return cell & COLOR_MASK;
}

void set_cell_color(CELL *cell, uint32_t color)
{
    // Vérifie que la couleur n'écrit pas sur les murs :
    assert (color & WALLS_MASK == 0);

    *cell &= WALLS_MASK
    *cell |= color;
}

bool is_wall(CELL cell, WALL wall_type)
{
    return cell & wall;
}

void set_wall(CELL *cell, WALL wall_type)
{
    *cell |= wall;
}

void remove_wall(CELL *cell, WALL wall_type)
{
    *cell &= ~wall;
}

void labyrinth_generator(void)
{
    
}
