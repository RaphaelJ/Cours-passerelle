/* Programming techniques: Symbol tables.
 * Marien Bourguignon & Raphael Javaux - December 2012.
 *
 * This file defines a dynamic hash table with linear probing.
 */

#ifndef HASHTABLE_H
#define HASHTABLE_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "st.h"

/** Defines the of the hash table's vector initial size.
 */
const size_t HT_INIT_SIZE;

/** Defines the maximum usage (between 0 and 1) of an hash table before a
 * reallocation (default : 0.5).
 */
const double HT_MAX_USAGE;

/** Stores an item in the hash table with the occupation status of the file.
 */
typedef struct {
    bool used;
    item_t item;
} ht_cell_t;

/** Stores the hash table and its items.
 */
typedef struct {
    size_t size;
    size_t count;
    ht_cell_t *cells;
} ht_t;

/** Initialises an empty hash table.
 */
ht_t *HTInit(void);

/** Inserts an item in the hash table. Returns the new hash table.
 */
ht_t *HTInsert(ht_t *ht, item_t item);

/** Returns true if the item is in the hash table.
 */
bool HTSearch(const ht_t *ht, item_t item);

/** Frees the tree and all of its nodes.
 */
void HTFree(ht_t *ht);

#endif
