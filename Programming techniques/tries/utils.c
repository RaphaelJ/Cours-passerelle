/* Programming techniques: Symbol tables.
 * Marien Bourguignon & Raphael Javaux - December 2012.
 *
 * This file defines simple utilities to handle common errors.
 */

#include "utils.h"

void* m_malloc(size_t size)
{
    void* mem = malloc(size);
    if (mem)
        return mem;
    die("Unable to allocate memory.");
    return NULL; //Remove the warning
}

void die(char* msg) 
{
    fprintf(stderr, "%s\n", msg);
    exit(EXIT_FAILURE);
}
