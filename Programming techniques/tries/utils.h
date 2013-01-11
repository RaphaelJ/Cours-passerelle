/* Programming techniques: Symbol tables.
 * Marien Bourguignon & Raphael Javaux - December 2012.
 *
 * This file defines simple utilities to handle common errors.
 */

#ifndef _UTILS_H
#define _UTILS_H

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/** Allocates a vector of the given size in bytes. Exits the program if the
 * allocation fails.
 */
void *m_malloc(size_t size);

/** Prints the message on stderr and exits the program.
 */
void die(char *msg);

#endif
