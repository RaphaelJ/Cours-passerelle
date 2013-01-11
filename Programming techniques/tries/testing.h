/* Programming techniques: Symbol tables.
 * Marien Bourguignon & Raphael Javaux - December 2012.
 *
 * This file defines a set of functions to test the different symbol tables.
 */

#ifndef _TESTING_H
#define _TESTING_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "st.h"

/** Sets the number of items to insert in the symbol tables during the tests.
 */
extern const int BOUND;

/** Defines the different kind of insertion methods.
 * REVERSE_INC : inserts all the number in [BOUND-1..0] ;
 * RANDOM : inserts BOUND random numbers ;
 * INCREMENTAL : inserts all the number in [0..BOUND-1].
 */
typedef enum {
    REVERSE_INC = 0, RANDOM = 1, INCREMENTAL = 2
} generation_method_t;

/** Defines the different kind of lookup methods.
 * HIT : looks for value in the symbol table ;
 * MISS : looks for values NOT in the symbol table.
 */
typedef enum { HIT = 0, MISS = 1 } t_lookup_type;

/** Inserts BOUND values in the symbol table using a generic interface and
 * the given method of generation for the values.
 */
void *insertValues(STInsert insertFct, void *st, generation_method_t type);

/** Looks for BOUND values in the symbol table using a generic interface and
 * the given kind of items which must be searched.
 * If ignore_missing is true, doesn't ensures that the item is in the table
 * when looking for existing keys.
 */
void searchValues(
    STSearch searchFct, void *st, t_lookup_type type, bool ignore_missing
);

#endif
