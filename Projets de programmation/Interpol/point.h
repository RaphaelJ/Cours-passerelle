/* Projet de programmation: Interpolation Numerique
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit les structures et fonctions utilisees pour les points.
 */

#ifndef POINT_H
#define POINT_H

#include <stdio.h>
#include <stdlib.h>

/** Contient les coordonnees d'un point.
 */
typedef struct {
   double x, y;
} Point;

/** Contient les coordonnees d'un point avec la derivee locale.
 */
typedef struct {
   double x, y, m;
} PointDer;

/** Trie les n points ps par ordre croissant d'abcisse.
 */
void pointsSort(int n, Point ps[]);

/** Compare deux points par leur abcisse.
 * @post Retourne un nombre negatif si x1 < x2, 0 si x1 == x2 et un nombre
 *       positif si x1 > x2.
 */
int pointCmp(const void *p1, const void *p2);

#endif
