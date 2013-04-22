#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/**
 * Precomputes coordinates for each dimension of the board.
 */
typedef struct {
    int *prev, *next;
} index_t;

char *next_state(const char *board, int size);
inline static char next_cell_state(
    const index_t index, const char *board, int size, int x, int y
);
static char *allocate_board(int size);
static char *randomBoard(int size);

int main(int argc, char *argv[])
{
    if (argc != 2)
        printf("USAGE: %s <board size>\n", argv[0]);
    else {
        int size = atoi(argv[1]);
        srand(0); // Deterministic seed.

        char *board = randomBoard(size);

        // Executes 100 times a generator and print the average time per sample.
        clock_t start = clock();

        for (int i = 0; i < 100; i++)
            next_state(board, size);

        double millisec = ((double) (clock() - start) / CLOCKS_PER_SEC) * 1000;
        printf("Sequential execution time: %f ms\n", millisec / 100);
    }

    return 0;
}

/**
 * Computes the next state of the entire board.
 */
char *next_state(const char *board, int size)
{
    char *res = allocate_board(size);
    index_t index;

    // Precomputes indices.
    index.prev = malloc(sizeof (int) * size);
    index.prev[0] = size - 1;
    for (int i = 1; i < size; i++)
        index.prev[i] = i - 1;

    index.next = malloc(sizeof (int) * size);
    index.next[size - 1] = 0;
    for (int i = 0; i < size - 1; i++)
        index.next[i] = i + 1;

    // Computes the new state.
    for (int y = 0; y < size; y++) {
        int dy = y * size;
        for (int x = 0; x < size; x++)
            res[dy + x] = next_cell_state(index, board, size, x, y);
    }

    return res;
}

/**
 * Computes the next state of a cell. Use the coordinates lookup table
 * to avoid multiple computations of the same neighbours indexes.
 */
inline static char next_cell_state(
    const index_t index, const char *board, int size, int x, int y
)
{
    int x1 = index.prev[x]
      , x2 = index.next[x]
      , y1 = index.prev[y]
      , y2 = index.next[y];

    int dy1 = y1 * size
      , dy  = y  * size
      , dy2 = y2 * size;

    int n = board[dy1 + x1] + board[dy1 + x] + board[dy1 + x2]
          + board[dy  + x1] +                  board[dy  + x2]
          + board[dy2 + x1] + board[dy2 + x] + board[dy2 + x2];

    if  (n == 3 || (n == 2 && board[dy + x] == 1))
        return 1;
    else
        return 0;
}

static char *allocate_board(int size)
{
    return malloc(sizeof (char) * size * size);
}

/**
 * Generates a random board with 1 and 0 values.
 */
static char *randomBoard(int size)
{
    char *board = allocate_board(size);

    for (int y = 0; y < size; y++) {
        int dy = y * size;
        for (int x = 0; x < size; x++)
            board[dy + x] = rand() % 1;
    }

    return board;
}
