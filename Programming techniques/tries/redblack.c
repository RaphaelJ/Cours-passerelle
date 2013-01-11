/* Programming techniques: Symbol tables.
 * Marien Bourguignon & Raphael Javaux - December 2012.
 *
 * This file defines a Red Black Tree storing item_t.
 */

#include "redblack.h"
 
/** Used to swith the root and one of its children while preserving BST ordering.
 */
static rbt_t *rotR(rbt_t *h);
static rbt_t *rotL(rbt_t *h);

/** Stores the recursive architecture of the tree.
 */
static rbt_t *RBTInsertRec(rbt_t *h, item_t item, int sw);

rbt_t *RBTInit(void)
{
    return NULL;
}

rbt_t *RBTInsert(rbt_t *tree, item_t item)
{
    rbt_t* ret = RBTInsertRec(tree, item, 0);
    ret->red = false;
    return ret;
}

static rbt_t *RBTInsertRec(rbt_t *h, item_t item, int sw)
{
    Key v = key(item);

    if (h == NULL) {
        rbt_t *node = m_malloc(sizeof (rbt_t));
        node->item = item;
        node->red = 1;
        node->left = node->right = NULL;
        return node;
    }

    if (chl && chr) {
        h->red = 1;
        hl->red = 0;
        hr->red = 0;
    }

    if (less(v, key(h->item))) { // Inserts on the left
        hl = RBTInsertRec(hl, item, 0);
        if (h->red && chl && sw)
            h = rotR(h);

        if(chl && chll) {
            h = rotR(h);
            h->red = false;
            hr->red = true;
        }
    } else {                    // Inserts on the right
        h->red = true; 
        hr = RBTInsertRec(hr, item, 1);
        if (h->red && chr && !sw)
                h = rotL(h);

        if (chr && chrr) {
            h = rotL(h);
            h->red = false;
            hl->red = true;
        }
    }

    return h;
}

bool RBTSearch(const rbt_t *tree, item_t item)
{
    if (tree == NULL)
        return false;
    else if (eq(item, tree->item))
        return true;
    else if (less(item, tree->item))
        return RBTSearch(tree->left, item);
    else
        return RBTSearch(tree->right, item);
}

void RBTFree(rbt_t *tree)
{
    if (tree != NULL) {
        RBTFree(tree->left);
        RBTFree(tree->right);
        free(tree);
    }
}

static rbt_t *rotL(rbt_t *h)
{
    rbt_t *x = h->right; h->right = x->left; x->left = h;
    return x; 
}

static rbt_t *rotR(rbt_t *h)
{
    rbt_t *x = h->left; h->left = x->right; x->right = h;
    return x;
}
