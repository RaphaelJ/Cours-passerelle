/* Projet de programmation: Calculs sur des entiers en precision arbitraire.
 * Raphael Javaux - Septembre 2012.
 *
 * Ce fichier contient toutes les operations arithmetiques (+, -, * et /)
 * utilisables sur les entiers a precision variables.
 */

#ifndef ARITHMETIC_H
#define ARITHMETIC_H

#include <stdio.h>
#include <stdlib.h>

#include "integers.h"

/** @pre Deux entiers a precision variable valides ;
 * @post Un nouvel entier resultat de l'addition des deux nombres.
 */
Integer addition(Integer e1, Integer e2);

/** @pre Deux entiers a precision variable valides ;
 * @post Un nouvel entier resultat de la soustraction du premier entier par le
 *       second entier.
 */
Integer subtraction(Integer e1, Integer e2);

/** @pre Deux entiers a precision variable valides ;
 * @post Un nouvel entier resultat de la multiplication du premier entier par le
 *       second entier.
 */
Integer multiplication(Integer e1, Integer e2);

/** @pre Deux entiers a precision variable valides. Le denominateur ne peut
 *       etre nul ;
 * @post Un nouvel entier resultat de la division du premier entier par le
 *       second entier.
 */
Integer division(Integer numerator, Integer denominator);

#endif
