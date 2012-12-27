/* Programming techniques: Symbol tables.
 * Marien Bourguignon & Raphael Javaux - December 2012.
 *
 * Tests some simple operations on the different symbol table algorithms.
 */

#include <stdio.h>
#include <stdlib.h>

#include <assert.h>

#include "st.h"

#include "bst.h"
#include "hashtable.h"
#include "tst.h"
// #include "redblack.h"

/** Tests if the given functions which describe a symbol table passes some
 * simple tests.
 */
static void testST(
    STInit initFct, STInsert insertFct, STSearch searchFct, STFree freeFct
);

int main(void)
{
    testST(
        (STInit) BSTInit, (STInsert) BSTInsert, (STSearch) BSTSearch,
        (STFree) BSTFree
    );
//     testST(
//         (STInit) RBInit, (STInsert) RBInsert, (STSearch) RBSearch,
//         (STFree) RBFree
//     );
    testST(
        (STInit) HTInit, (STInsert) HTInsert, (STSearch) HTSearch, 
        (STFree) HTFree
    );
    testST(
        (STInit) TSTInit, (STInsert) TSTInsert, (STSearch) TSTSearch,
        (STFree) TSTFree
    );

    return 0;
}

static void testST(
    STInit initFct, STInsert insertFct, STSearch searchFct, STFree freeFct
)
{
    void *st = initFct();

    assert (!searchFct(st, 5418));
    st = insertFct(st, 5418);
    assert (!searchFct(st, 45784));
    assert (searchFct(st, 5418));

    freeFct(st);
}
