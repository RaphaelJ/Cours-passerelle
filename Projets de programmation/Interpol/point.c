/* Projet de programmation: Interpolation Numerique
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit les structures et fonctions utilisees pour les points.
 */

#include "point.h"

void pointsSort(int n, Point ps[])
{
   qsort(ps, n, sizeof (Point), pointCmp);
}

int pointCmp(const void *p1, const void *p2)
{
   return ((Point *) p1)->x - ((Point *) p2)->x;
}
