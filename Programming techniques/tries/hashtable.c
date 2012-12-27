/* Programming techniques: Symbol tables.
 * Marien Bourguignon & Raphael Javaux - December 2012.
 *
 * This file defines a dynamic hash table with linear probing.
 */

#include "hashtable.h"

const size_t HT_INIT_SIZE = 8;
const double HT_MAX_USAGE = 0.5;

/** Computes the index of an item in the hash table.
 */
static size_t HTHash(const ht_t *ht, item_t item);

/** Inserts the item in the hash table in the first free space (linear probing)
 * without checking if there is still empty spaces in the vector.
 */
static void HTInsertProbe(ht_t *ht, const item_t item);

/** Reallocates the internal vector of the hash table to double its size.
 */
static void HTReallocate(ht_t *ht);

ht_t *HTInit(void)
{
    ht_t *ht = malloc(sizeof (ht_t));
    ht->size = HT_INIT_SIZE;
    ht->count = 0;
    ht->cells = calloc(sizeof (ht_cell_t), HT_INIT_SIZE);

    return ht;
}

ht_t *HTInsert(ht_t *ht, item_t item)
{
    ht->count++;
    if ((double) ht->count / (double) ht->size > HT_MAX_USAGE)
        HTReallocate(ht);

    HTInsertProbe(ht, item);

    return ht;
}

bool HTSearch(const ht_t *ht, item_t item)
{
    size_t index = HTHash(ht, item);

    while (ht->cells[index].used) {
        if (ht->cells[index].item == item)
            return true;
        index++;
    }
    return false;
}

void HTFree(ht_t *ht)
{
    free(ht->cells);
    free(ht);
}

static size_t HTHash(const ht_t *ht, item_t item)
{
    return item % ht->size;
}

static void HTInsertProbe(ht_t *ht, const item_t item)
{
    size_t index = HTHash(ht, item);

    // Skips non-empty cells.
    while (ht->cells[index].used)
        index++;

    ht->cells[index] = (ht_cell_t) { .used = true, .item = item };
}

static void HTReallocate(ht_t *ht)
{
    size_t old_size = ht->size;
    ht->size = old_size * 2;

    ht_cell_t *old_cells = ht->cells;
    ht->cells = calloc(sizeof (ht_cell_t), ht->size);

    // Reinserts existing items in the new vector.
    for (size_t i = 0; i < old_size; i++) {
        if (old_cells[i].used)
            HTInsertProbe(ht, old_cells[i].item);
    }

    free(old_cells);
}
