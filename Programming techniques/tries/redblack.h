/* Programming techniques: Symbol tables.
 * Marien Bourguignon & Raphael Javaux - December 2012.
 *
 * This file defines a Red Black Tree storing item_t.
 */
 
#ifndef _RBT_H
#define _RBT_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "st.h"
#include "utils.h"

/** Define used for accessing sub trees in a classiest way.
 */
#define hr h->right
#define hrr h->right->right
#define hl h->left
#define hll h->left->left

/** Check if the right/left tree (one and two levels above) does exist.
 */
#define chr ((hr)? hr->red: false)
#define chl ((hl)? hl->red: false)
#define chrr ((hrr)? hrr->red: false)
#define chll ((hll)? hll->red: false)

/** Defines used for manipulating the data structure.
 */
#define key(A)          (A)
#define less(A, B)      (key(A) < key(B))
#define eq(A, B)        (key(A) == key(B))
#define nn(A)           (A != NULL)

typedef item_t Key;

/** Stores the recursive architecture of the tree.
 */
typedef struct _rbt_t {
    item_t item;
    struct _rbt_t *left, *right;
    bool red;
} rbt_t;

/** Initialises an empty Red Black Tree.
 */
rbt_t *RBTInit(void);

/** Inserts an item in the Red Black Tree. Returns the new RBT.
 */
rbt_t *RBTInsert(rbt_t *tree, item_t item);

/** Returns true if the item is in the Red Black Tree.
 */
bool RBTSearch(const rbt_t *tree, item_t item);

/** Frees the tree and all of its nodes.
 */
void RBTFree(rbt_t *tree);

#endif
