/* Programming techniques: Symbol tables.
 * Marien Bourguignon & Raphael Javaux - December 2012.
 *
 * This file defines a set of functions to test the different symbol tables.
 */

#include "testing.h"

#include <assert.h>

const int BOUND = 14000;

void *insertValues(STInsert insertFct, void *st, generation_method_t type)
{
    switch(type) {
    case REVERSE_INC:
        for (int i=BOUND-1; i>=0; i--)
            st = insertFct(st, i);
        break;
    case RANDOM:
        for (int i=0; i<BOUND; i++)
            st = insertFct(st, rand()%BOUND);
        break;
    case INCREMENTAL:
        for (int i=0; i<BOUND; i++)
            st = insertFct(st, i);
        break;
    }
    return st;
}

void searchValues(
    STSearch searchFct, void *st, t_lookup_type type, bool ignore_missing
)
{
    if (type == HIT) { // Lookup for values in the symbol table
        for (int i=0; i<BOUND; i++)
            assert (ignore_missing || searchFct(st, i));
    } else {
        // Lookup for values NOT in the symbol table. As the values inserted
        // are in the range [0..BOUND-1], we are looking for values in
        // [-1..-BOUND] and [BOUND..2 BOUND - 1] (to look at both sides of
        // tree-based data structures).
        for (int i=0; i<BOUND; i++) {
            if (i % 2 == 0) // In [-1..-BOUND]
                assert (!searchFct(st, -1 - i));
            else // In [BOUND..2 BOUND - 1]
                assert (!searchFct(st, BOUND + i));
        }
    }
}
