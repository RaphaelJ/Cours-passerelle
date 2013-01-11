/* Programming techniques: Symbol tables.
 * Marien Bourguignon & Raphael Javaux - December 2012.
 *
 * Tests some simple operations on the different symbol table algorithms.
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include <sys/time.h>

#include "st.h"

#include "bst.h"
#include "hashtable.h"
#include "tst.h"
#include "redblack.h"
#include "testing.h"

/** Returns the current time in millisecond.
 */
static double getTimeInMilli(void);

/** Tests if the given functions which describe a symbol table success to some
 * simple tests and measures the processing time.
 */
static void testST(
    STInit initFct, STInsert insertFct, STSearch searchFct, STFree freeFct, 
    const char *name
);

int main(void)
{
    // Initialises the salt to a constant number so we run in a deterministic
    // system.
    srand(1);

    testST(
        (STInit) BSTInit, (STInsert) BSTInsert, (STSearch) BSTSearch,
        (STFree) BSTFree, "BST"
    );
    testST(
        (STInit) RBTInit, (STInsert) RBTInsert, (STSearch) RBTSearch,
        (STFree) RBTFree, "Red Black Tree"
    );
    testST(
        (STInit) HTInit, (STInsert) HTInsert, (STSearch) HTSearch, 
        (STFree) HTFree, "Hash Table"
    );
    testST(
        (STInit) TSTInit, (STInsert) TSTInsert, (STSearch) TSTSearch,
        (STFree) TSTFree, "TST"
    );

    return 0;
}

static void testST(
    STInit initFct, STInsert insertFct, STSearch searchFct, STFree freeFct,
    const char *name
)
{
    printf("Tests the %s symbol table with %d items...\n", name, BOUND);

    for(int i=REVERSE_INC; i<=INCREMENTAL; i++)
    {
        switch (i) {
        case REVERSE_INC:
            printf("\tReverse incremental test\n");
            break;
        case RANDOM:
            printf("\tRandom test\n");
            break;
        case INCREMENTAL:
            printf("\tIncremental test\n");
            break;
        }

        void *st = initFct();

        // Inserts the values in the symbol table.
        double before = getTimeInMilli();
        st = insertValues(insertFct, st, i);

        printf("\t\tInsertion: %f ms\n", getTimeInMilli() - before);

        // Retrieves the values from the symbol table.
        for(int k=HIT; k<=MISS; k++) {
            before = getTimeInMilli();
            searchValues(searchFct, st, k, i == RANDOM);
            double after = getTimeInMilli();

            if(k == HIT) {
                printf(
                    "\t\tSearch for existing items: %f ms\n", after - before
                );
            } else {
                printf(
                    "\t\tSearch for missing items: %f ms\n", after - before
                );
            }
        }

        freeFct(st);
    }
}

static double getTimeInMilli(void) 
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return ((tv.tv_sec) * 1000 + tv.tv_usec/1000.0) + 0.5;
}
