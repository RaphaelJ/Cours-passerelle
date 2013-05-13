/* Teste une implémentation d'un jeu de la vie écrit en Coda. */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "coda.h"

#define BOARD_WIDTH  15
#define BOARD_HEIGHT 10

void next_state(const cint_t *origin, cint_t *res);
void print_board(const cint_t* board);

int main(int argc, char *argv[])
{
    cint_t board_a[BOARD_HEIGHT][BOARD_WIDTH] = {
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        { 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0 },
        { 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0 },
        { 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 },
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        { 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0 },
        { 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0 },
        { 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0 },
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
    };
    cint_t board_b[BOARD_HEIGHT][BOARD_WIDTH];

    cint_t *origin = (cint_t *) board_a
         , *dest   = (cint_t *) board_b;

    for (;;) {
        next_state(origin, dest);

        system("clear");
        print_board(dest);
        sleep(1);

        cint_t *tmp = origin;
        origin = dest;
        dest = tmp;
    }

    return 0;
}

void print_board(const cint_t* board)
{
    for (int y = 0; y < BOARD_HEIGHT; y++) {
        for (int x = 0; x < BOARD_WIDTH; x++) {
            if (board[y * BOARD_WIDTH + x] == 1)
                printf("■");
            else
                printf("□");
        }
        putchar('\n');
    }
    putchar('\n');
}
