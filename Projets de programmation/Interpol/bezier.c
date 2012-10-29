/* Projet de programmation: Interpolation Numerique
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit les algorithmes et les structures pour tracer une courbe
 * de Bezier.
 */

#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include "bezier.h"

/** Trace m points d'une courbe de Bezier a partir d'un ensemble ps de n points.
 * @pre Un tableau ps de n points. n > 0 et m >= 0 ;
 * @post curve contient les m points de la courbe de Bezier.
 *       Le vecteur ps est utilise et modifie par la fonction.
 */
static void bezierCurveRec(int n, Point ps[], int m, Point curve[]);

Point *bezierCurve(int n, const Point ps[], int m)
{
   assert (n > 0);
   assert (m >= 0);

   /* Copie le tableau de points car celui-ci va etre modifies par les appels
    * recursifs. */
   Point *psCopy = (Point *) malloc(sizeof (Point) * n);
   assert (psCopy != NULL);
   memcpy(psCopy, ps, sizeof (Point) * n);

   // Alloue le tableau qui va contenir la courbe
   Point *curve = (Point *) malloc(sizeof (Point) * m);
   assert (curve != NULL);

   // Lance les appels recursifs pour accumuler les points a tracer
   bezierCurveRec(n, psCopy, m, curve);
   free(psCopy);

   return curve;
}

static void bezierCurveRec(int n, Point ps[], int m, Point curve[])
{
   if (m > 0) // Continue a tracer des points
   {
      /* Alloue un matrice pour les points intermediaires et initialise la
       * premiere ligne avec les points donnes en parametre. */
      Point *matrix = (Point *) malloc(sizeof (Point) * n * n);
      assert (matrix != NULL);
      memcpy(matrix, ps, sizeof (Point) * n);

      Point *linePrec = matrix;
      for (int i = 1; i < n; i++)
      {
         Point *line = matrix + (i * n);
         for (int j = 0; j < (n - i); j++)
         {
            /* Chaque point de matrix[i][j] est le milieu de matrix[i-1][j] et
             * matrix[i-1][j+1]. */
            line[j].x = (linePrec[j].x + linePrec[j + 1].x) / 2;
            line[j].y = (linePrec[j].y + linePrec[j + 1].y) / 2;
         }

         linePrec = line;
      }

      /* Ajoute le dernier point calcule au milieu de la matrice des points a
       * tracer. Ainsi, les points seront ordonnes a la fin de l'execution. */
      int middle = m / 2;
      curve[middle] = linePrec[0];
      m--;

      /* Trace les m points restants en recursant sur les deux sous ensembles.
       * Chacun des deux sous-ensembles va tracer la moitie des points restants
       * a tracer. */

      /* Recurse sur l'ensemble ps[i] = matrix[i][0] et ajoute a la gauche du
       * tableau de trace. */
      for (int i = 0; i < n; i++)
         ps[i] = *(matrix + i * n);

      int mLeft = m / 2 + m % 2;
      bezierCurveRec(n, ps, mLeft, curve);

      /* Recurse sur l'ensemble ps[i] = matrix[n-1-i][i] et ajoute a la gauche
       * du tableau de trace. */
      for (int i = 0; i < n; i++)
         ps[i] = *(matrix + (n - 1 - i) * n + i);

      free(matrix);

      bezierCurveRec(n, ps, m / 2 , curve + middle + 1);
   }
}
