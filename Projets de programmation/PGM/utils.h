/* Projet de programmation: Gestion d’Images Monochromes
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit quelques fonctions utilitaires.
 */

#ifndef UTILS_H
#define UTILS_H

#include <stdio.h>
#include <stdlib.h>

/** @post a si a < b, b sinon.
 */
inline int min(int a, int b)
{
    return a < b ? a : b;
}

/** @post a si a > b, b sinon.
 */
inline int max(int a, int b)
{
    return a > b ? a : b;
}

#endif
