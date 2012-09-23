/* Projet de programmation: Calculs sur des entiers en precision arbitraire.
 * Raphael Javaux - Septembre 2012.
 *
 * Ce fichier contient des wrappers vers les fonctions du projets qui sont
 * uniquement utilises par le programme de test unitaire ecrit en Haskell qui
 * impose que toutes les donnees passees au programme C le soient sous forme
 * de pointeurs.
 * Les fonctions ci-dessous sont donc de simple wrappers vers les fonctions
 * du meme nom du projet mais qui retournent et prennent des pointeurs vers des 
 * entiers.
 */

#ifndef UNITTESTSWRAPPER_H
#define UNITTESTSWRAPPER_H

#include <stdio.h>
#include <stdlib.h>

#include "../integers.h"
#include "../comparaisons.h"
#include "../arithmetic.h"

Integer *d_readInteger(char *str);
void d_showInteger(Integer *integer);

int d_compare(Integer *i1, Integer *i2);

Integer *d_addition(Integer *i1, Integer *i2);
Integer *d_subtraction(Integer *i1, Integer *i2);
Integer *d_multiplication(Integer *i1, Integer *i2);
Integer *d_division(Integer *numerator, Integer *denominator);

#endif
