/* Computation Structures: Générateur de labyrinthes parallèle
 * Raphael Javaux - Novembre 2012.
 *
 * Ce fichier définit un algorithme pour générer des labyrinthes de 12 cases de
 * coté ainsi que la structure de données associée.
 */

#include <assert.h>

#include "labyrinth.h"

const int LABYRINTH_SIZE = 12;

const uint32_t COLOR_MASK = 0x0FFFFFFF;
const uint32_t WALLS_MASK = 0xF0000000;

// Chaque mur est associé à un masque lié à son bit.
const WALL WALL_TOP    = 0x80000000;
const WALL WALL_RIGHT  = 0x40000000;
const WALL WALL_BOTTOM = 0x20000000;
const WALL WALL_LEFT   = 0x10000000;

uint32_t cell_color(CELL cell)
{
    return cell & COLOR_MASK;
}

void set_cell_color(CELL *cell, uint32_t color)
{
    // Vérifie que la couleur n'écrit pas sur les murs :
    assert ((color & WALLS_MASK) == 0);

    *cell &= WALLS_MASK;
    *cell |= color;
}

bool is_wall(CELL cell, WALL wall_type)
{
    return cell & wall_type;
}

void set_wall(CELL *cell, WALL wall_type)
{
    *cell |= wall_type;
}

void remove_wall(CELL *cell, WALL wall_type)
{
    *cell &= ~wall_type;
}

LABYRINTH init_labyrinth(void)
{
    CELL *labyrinth = (CELL *) malloc(
        sizeof (CELL) * LABYRINTH_SIZE * LABYRINTH_SIZE
    );

    for (int i = 0; i < LABYRINTH_SIZE * LABYRINTH_SIZE; i++)
        labyrinth[i] = WALLS_MASK | i;

    return labyrinth;
}

void show_labyrinth(const LABYRINTH labyrinth)
{
    for (int ln = 0; ln < LABYRINTH_SIZE; ln++) {
        const CELL *line = labyrinth + ln * LABYRINTH_SIZE;

        // Dessine les bordures supérieures de la ligne.
        for (int col = 0; col < LABYRINTH_SIZE; col++) {
            if (is_wall(line[col], WALL_TOP))
                printf(" ―");
            else
                printf("  ");
        }
        putchar('\n');

        // Dessine les bordures latérales des cellules de la ligne
        for (int col = 0; col < LABYRINTH_SIZE; col++) {
            if (is_wall(line[col], WALL_LEFT))
                printf("| ");
            else
                printf("  ");
        }
        if (is_wall(line[LABYRINTH_SIZE - 1], WALL_RIGHT))
            putchar('|');

        putchar('\n');
    }

    // Dessine la bordure inférieure de la dernière ligne
    const CELL *last = labyrinth + LABYRINTH_SIZE * (LABYRINTH_SIZE - 1);
    for (int col = 0; col < LABYRINTH_SIZE; col++) {
        if (is_wall(last[col], WALL_BOTTOM))
            printf(" ―");
        else
            printf("  ");
    }
    putchar('\n');
}

// void gen_labyrinth()
// {
//     
// }
