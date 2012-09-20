#ifndef COMPARAISONS_H
#define COMPARAISONS_H

#include <stdio.h>
#include <stdlib.h>

#include "entiers.h"

/** @pre Deux nombre entiers valides a comparer.
 *  @post Un entier negatif si le premier entier est inferieur au second, 0 si 
 *        les deux nombres sont egaux, un entier positif si le second nombre est
 *        plus petit.
 */
int comparer(Entier e1, Entier e2);

/** @pre Deux nombre entiers valides.
 *  @post 1 si le premier entier est inferieur au second, 0 sinon.
 */
int inferieur(Entier e1, Entier e2);

/** @pre Deux nombre entiers valides.
 *  @post 1 si le premier entier est inferieur ou egal au second, 0 sinon.
 */
int inferieurEgal(Entier e1, Entier e2);

/** @pre Deux nombre entiers valides.
 *  @post 1 si le premier entier est plus grand que le second, 0 sinon.
 */
int superieur(Entier e1, Entier e2);

/** @pre Deux nombre entiers valides.
 *  @post 1 si le premier entier est plus grand ou egal au second, 0 sinon.
 */
int superieurEgal(Entier e1, Entier e2);

/** @pre Deux nombre entiers valides.
 *  @post 1 si le premier entier est egal au second, 0 sinon.
 */
int egal(Entier e1, Entier e2);

/** @pre Deux nombre entiers valides.
 *  @post 1 si le premier entier est different du second, 0 sinon.
 */
int different(Entier e1, Entier e2);

#endif
