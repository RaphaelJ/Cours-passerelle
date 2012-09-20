#ifndef ARITHMETIQUE_H
#define ARITHMETIQUE_H

#include <stdio.h>
#include <stdlib.h>

#include "entiers.h"

/** @pre Deux entiers a precision variable valides.
 * @post Un nouvel entier resultat de l'addition des deux nombres.
 */
Entier addition(Entier e1, Entier e2);

/** @pre Deux entiers a precision variable valides.
 * @post Un nouvel entier resultat de la soustraction du premier entier par le
 *       second entier.
 */
Entier soustraction(Entier e1, Entier e2);

/** @pre Deux entiers a precision variable valides.
 * @post Un nouvel entier resultat de la multiplication du premier entier par le
 *       second entier.
 */
Entier multiplication(Entier e1, Entier e2);

#endif
