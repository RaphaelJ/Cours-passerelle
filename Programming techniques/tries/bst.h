/* Programming techniques: Symbols tables.
 * Marien Bourguignon & Raphael Javaux - December 2012.
 *
 * This file defines a Binary Search Tree storing item_t.
 */

#ifndef BST_H
#define BST_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "st.h"

/** Stores the recursive architecture of the tree.
 */
typedef struct _bst_t {
    item_t item;
    struct _bst_t *left, *right;
} bst_t;

/** Initialises an empty Binary Search Tree.
 */
bst_t *BSTInit(void);

/** Inserts an item in the Binary Search Tree. Returns the new BST.
 */
bst_t *BSTInsert(bst_t *tree, item_t item);

/** Returns true if the item is in the Binary Search Tree.
 */
bool BSTSearch(const bst_t *tree, item_t item);

/** Frees the tree and all of its nodes.
 */
void BSTFree(bst_t *tree);

#endif
