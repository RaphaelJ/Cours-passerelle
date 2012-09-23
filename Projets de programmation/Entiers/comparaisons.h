/* Projet de programmation: Calculs sur des entiers en precision arbitraire.
 * Raphael Javaux - Septembre 2012.
 *
 * Ce fichier contient toutes les operations de comparaison (compare, <, <=, >,
 * >=, == et !=) utilisables sur les entiers a precision variables.
 */

#ifndef COMPARAISONS_H
#define COMPARAISONS_H

#include <stdio.h>
#include <stdlib.h>

#include "integers.h"

/** @pre Deux nombre entiers valides a comparer ;
 *  @post Un entier negatif si le premier entier est inferieur au second, 0 si
 *        les deux nombres sont egaux, un entier positif si le second nombre est
 *        plus petit.
 */
int compare(Integer e1, Integer e2);

/** @pre Deux nombre entiers valides ;
 *  @post 1 si le premier entier est inferieur au second, 0 sinon.
 */
int inferior(Integer e1, Integer e2);

/** @pre Deux nombre entiers valides ;
 *  @post 1 si le premier entier est inferieur ou egal au second, 0 sinon.
 */
int inferiorEquals(Integer e1, Integer e2);

/** @pre Deux nombre entiers valides ;
 *  @post 1 si le premier entier est plus grand que le second, 0 sinon.
 */
int superior(Integer e1, Integer e2);

/** @pre Deux nombre entiers valides ;
 *  @post 1 si le premier entier est plus grand ou egal au second, 0 sinon.
 */
int superiorEquals(Integer e1, Integer e2);

/** @pre Deux nombre entiers valides ;
 *  @post 1 si le premier entier est egal au second, 0 sinon.
 */
int equals(Integer e1, Integer e2);

/** @pre Deux nombre entiers valides ;
 *  @post 1 si le premier entier est different du second, 0 sinon.
 */
int different(Integer e1, Integer e2);

/** @pre Un entier valide ;
 *  @post Un entier non nul si l'entier est nul.
 */
bool isZero(Integer integer);

/** @pre Un entier valide ;
 *  @post Un entier non nul si l'entier est positif et non nul.
 */
bool isPositive(Integer integer);

#endif
