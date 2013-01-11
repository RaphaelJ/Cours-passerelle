/* Programming techniques: Symbol tables.
 * Marien Bourguignon & Raphael Javaux - December 2012.
 *
 * This file defines a Ternary Search Tree storing item_t by converting these in
 * character strings.
 */

#ifndef _TST_H
#define _TST_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "st.h"

/** Contains the maximum length of a string encoded item_t.
 */
extern const size_t TST_BUFFER_SIZE;

/** Stores the recursive architecture of the tree.
 */
typedef struct _tst_t {
    char value;
    struct _tst_t *left, *middle, *right;
} tst_t;

/** Initialises an empty Ternary Search Tree.
 */
tst_t *TSTInit(void);

/** Inserts an item in the Ternary Search Tree. Returns the new TST.
 */
tst_t *TSTInsert(tst_t *tree, item_t item);

/** Returns true if the item is in the Ternary Search Tree.
 */
bool TSTSearch(const tst_t *tree, item_t item);

/** Frees the tree and all of its nodes.
 */
void TSTFree(tst_t *tree);

#endif
