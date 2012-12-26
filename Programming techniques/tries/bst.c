/* Programming techniques: Symbols tables.
 * Marien Bourguignon & Raphael Javaux - December 2012.
 *
 * This file defines a Binary Search Tree storing item_t.
 */

#include "bst.h"

bst_t *BSTInit(void)
{
    return NULL;
}

bst_t *BSTInsert(bst_t *tree, item_t item)
{
    if (tree == NULL) { // Empty leaf, inserts on place. Returns the new node.
        bst_t *node = malloc(sizeof (bst_t));
        node->item = item;
        node->left = node->right = NULL;
        return node;
    } else if (item < tree->item) {
        tree->left = BSTInsert(tree->left, item);
        return tree;
    } else if (item > tree->item) {
        tree->right = BSTInsert(tree->right, item);
        return tree;
    } else // Doesn't insert a key which already exists.
        return tree;
}

bool BSTSearch(const bst_t *tree, item_t item)
{
    if (tree == NULL)
        return false;
    else if (item == tree->item)
        return true;
    else if (item < tree->item)
        return BSTSearch(tree->left, item);
    else
        return BSTSearch(tree->right, item);
}

void BSTFree(bst_t *tree)
{
    if (tree != NULL) {
        BSTFree(tree->left);
        BSTFree(tree->right);
        free(tree);
    }
}
