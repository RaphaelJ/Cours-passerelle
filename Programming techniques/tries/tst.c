/* Programming techniques: Symbol tables.
 * Marien Bourguignon & Raphael Javaux - December 2012.
 *
 * This file defines a Ternary Search Tree storing item_t by converting these in
 * character strings.
 */

#include "tst.h"

const size_t TST_BUFFER_SIZE = sizeof "-2147483648";

/** Inserts the item encoded in the buffer in the tree.
 */
static tst_t *TSTInsertRec(tst_t *tree, const char *buffer);

/** Searchs for an item encoded in the buffer in the tree.
 */
static bool TSTSearchRec(const tst_t *tree, const char *buffer);

/** Encodes the item in the given string.
 */
static void TSTEncodeItem(item_t item, char *buffer);

tst_t *TSTInit(void)
{
    return NULL;
}

tst_t *TSTInsert(tst_t *tree, item_t item)
{
    char buffer[TST_BUFFER_SIZE];
    TSTEncodeItem(item, buffer);

    return TSTInsertRec(tree, buffer);
}

static tst_t *TSTInsertRec(tst_t *tree, const char *buffer)
{
    if (tree == NULL) {
        tree = malloc(sizeof (tst_t));
        tree->value = *buffer;
        tree->left = tree->right = NULL;

        if (*buffer != '\0')
            tree->middle = TSTInsertRec(NULL, buffer + 1);
        else
            tree->middle = NULL;
    } else if (*buffer == '\0') {
        if (tree->value != '\0')
            tree->left = TSTInsertRec(tree->left, buffer);
    } else {
        if (tree->value > *buffer)
            tree->left = TSTInsertRec(tree->left, buffer);
        else if (tree->value == *buffer)
            tree->middle = TSTInsertRec(tree->middle, buffer + 1);
        else
            tree->right = TSTInsertRec(tree->right, buffer);
    }

    return tree;
}

bool TSTSearch(const tst_t *tree, item_t item)
{
    char buffer[TST_BUFFER_SIZE];
    TSTEncodeItem(item, buffer);

    return TSTSearchRec(tree, buffer);
}

static bool TSTSearchRec(const tst_t *tree, const char *buffer)
{
    if (tree == NULL)
        return false;
    else if (tree->value > *buffer)
        return TSTSearchRec(tree->left, buffer);
    else if (tree->value == *buffer) {
        if (*buffer == '\0') // Boths end the string.
            return true;
        else
            return TSTSearchRec(tree->middle, buffer + 1);
    } else 
        return TSTSearchRec(tree->right, buffer);
}

void TSTFree(tst_t *tree)
{
    if (tree != NULL) {
        TSTFree(tree->left);
        TSTFree(tree->middle);
        TSTFree(tree->right);

        free(tree);
    }
}

static void TSTEncodeItem(item_t item, char *buffer)
{
    sprintf(buffer, "%d", item);
}
