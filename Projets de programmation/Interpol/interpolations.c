/* Projet de programmation: Interpolation Numerique
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit les algorithmes et les structures permettant d'obtenir les
 * fonctions d'interpolations.
 */

#include "interpolations.h"

static double linearFct(InterpolData data, double x);
static double linearDestruct(InterpolData data);
static int linearSectionCmp(const void *section, const void *test);

static void *binarySearch(
     const void *arr, size_t nmemb, size_t size,
   , int (*compar)(const void *elem, const void *test)
);

Interpol linearInterpol(int n, const double xs[], const double ys[])
{
   assert (n >= 2);

   LinearData data = {
        .n = n - 1
      , sections = (LinearSection *) malloc(sizeof (LinearSection) * (n - 1))
   };
   assert (data.sections != NULL);

   // Calcule la pente de chaque section.
   for (int i = 0; i < (n - 1); i++)
   {
      double x1 = xs[i], x2 = xs[i + 1]
           , y1 = ys[i], y2 = ys[i + 1];
      assert (x2 > x1);

      data.sections[i] = {
         .x1 = x1, .x2 = x2, .y1 = y1, .m = (y2 - y1) / (x2 - x1)
      };
   }

   InterpolData genericData;
   genericData.linear = data;
   return (Interpol) {
      .data = genericData, .fct = linearFct, .destruct = linearDestruct
   };
}

static double linearFct(InterpolData data, double x)
{
   // x doit etre dans le domaine de l'interpolation.
   assert (
         x >= data.linear.sections[0].x1
      && x <= data.linear.sections[data.linear.n - 1].x2
   );

   LinearSection section = binarySearch(
      data.linear.sections, n, sizeof (LinearSection), linearSectionCmp
   );
   return section.m * (x - section.x1) + section.y1;
}

static double linearDestruct(InterpolData data)
{
   free(data.linear.sections);
}

static int linearSectionCmp(const void *section, const void *test)
{
   if (*test < section.x1)
      return -1;
   else if (*test > section.x2)
      return +1;
   else
      return 0;
}

splineInterpol(int n, const double *xs, const double *ys);
lagrangeInterpol(int n, const double *xs, const double *ys);
bezierInterpol(int n, const double *xs, const double *ys);

static void *binarySearch(
     const void *arr, size_t nmemb, size_t size,
   , int (*compar)(const void *elem, const void *test)
)
{
   
}