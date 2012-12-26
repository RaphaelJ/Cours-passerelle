/* Programming techniques: Symbols tables.
 * Marien Bourguignon & Raphael Javaux - December 2012.
 *
 * This file defines the type of the items which will be stored in the
 * different symbols tables and a common interface (defined with function
 * pointers) for each implementation.
 */

#ifndef ST_H
#define ST_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

/** Defines the type of the item stored in the symbol tables as a signed
 * integer.
 */
typedef int item_t;

// The three different actions that can be done on our symbols tables can be
// casted to the following three functions pointers :

/** Initialises a symbols table.
 */
typedef void *(*STInit)(void);

/** Inserts an item in a symbols table. Returns the new symbols table (this
 * makes easier to deal with tries and pointers). Does nothing if the item is 
 * already in the symbol table.
 */
typedef void *(*STInsert)(void *st, item_t item);

/** Returns true if the item is in the symbols table.
 */
typedef bool (*STSearch)(const void *st, item_t item);

/** Frees the memory used by the symbols table.
 */
typedef void (*STFree)(void *st);

#endif
