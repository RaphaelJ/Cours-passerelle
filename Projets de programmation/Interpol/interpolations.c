/* Projet de programmation: Interpolation Numerique
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit les algorithmes et les structures permettant d'obtenir les
 * fonctions d'interpolations.
 */

#include <string.h>

#include "interpolations.h"

/** @pre Les donnees d'interpolation lineaire et un point dans le domaine
 *       d'interpolation ;
 * @post le point a l'ordonnee correspondante estimee.
 */
static double linearFct(InterpolData data, double x);

/** @pre Les donnees de l'interpolation lineaire a liberer.
 */
static double linearDestruct(InterpolData data);

/** Comparateur utilise pour la recherche dicotomique de la section d'une
 * interpolation lineaire.
 * @pre Un pointeur vers une abscisse et une section de l'interpolation
 *      lineaire ;
 * @post un nombre negatif si l'abcsisse est avant le domaine de la section,
 *       un nombre nul si elle est dans le domaine et un nombre positif si
 *       elle est apres le domaine de la section.
 */
static int linearSectionCmp(const void *vX, const void *vSection);


/** @pre Les donnees d'interpolation par spline et un point dans le domaine
 *       d'interpolation ;
 * @post le point a l'ordonnee correspondante estimee.
 */
static double splineFct(InterpolData data, double x);

/** @pre Les donnees de l'interpolation par spline a liberer.
 */
static double splineDestruct(InterpolData data);

/** Comparateur utilise pour la recherche dicotomique de la section d'une
 * interpolation par spline.
 * @pre Un pointeur vers une abscisse et une section de l'interpolation
 *      par spline ;
 * @post un nombre negatif si l'abcsisse est avant le domaine de la section,
 *       un nombre nul si elle est dans le domaine et un nombre positif si
 *       elle est apres le domaine de la section.
 */
static int splineSectionCmp(const void *vX, const void *vSection);


/** @pre Les donnees d'interpolation de Lagrange et un point dans le domaine
 *       d'interpolation ;
 * @post le point a l'ordonnee correspondante estimee.
 */
static double lagrangeFct(InterpolData data, double x);

/** @pre Les donnees de l'interpolation de Lagrange a liberer.
 */
static double lagrangeDestruct(InterpolData data);


double interpolate(Interpol interpol, double x)
{
   return interpol.fct(interpol.data, x);
}

void freeInterpol(Interpol interpol)
{
   interpol.destruct(interpol.data);
}

Interpol linearInterpol(int n, const double xs[], const double ys[])
{
   assert (n >= 2);

   LinearData data = {
        .n = n - 1
      , sections = (LinearSection *) malloc(sizeof (LinearSection) * (n - 1))
   };
   assert (data.sections != NULL);

   // Calcule la pente de chaque section et copie les coordonnes.
   for (int i = 0; i < (n - 1); i++)
   {
      double x1 = xs[i], x2 = xs[i + 1]
           , y1 = ys[i], y2 = ys[i + 1];
      assert (x2 > x1);

      data.sections[i] = {
         .x1 = x1, .x2 = x2, .y1 = y1, .m = (y2 - y1) / (x2 - x1)
      };
   }

   return (Interpol) {
      .data.linear = data, .fct = linearFct, .destruct = linearDestruct
   };
}

static double linearFct(InterpolData data, double x)
{
   // Recherche la section par une recherche dicotomique.
   LinearSection *section = bsearch(
      &x, data.linear.sections, n, sizeof (LinearSection), linearSectionCmp
   );

   assert (section != NULL); // x doit etre dans le domaine de l'interpolation.

   return section->m * (x - section->x1) + section->y1;
}

static double linearDestruct(InterpolData data)
{
   free(data.linear.sections);
}

static int linearSectionCmp(const void *vX, const void *vSection)
{
   double x = *((double *) vX);
   LinearSection section = *((LinearSection *) vSection);

   if (x < section.x1)
      return -1;
   else if (x > section.x2)
      return +1;
   else
      return 0;
}

Interpol splineInterpol(
   int n, const double xs[], const double ys[], const double ms[]
)
{
   assert (n >= 2);

   SplineData data = {
        .n = n - 1
      , .sections = (SplineSection *) malloc(sizeof (SplineSection) * (n - 1))
   };
   assert (data.sections != NULL);

   // Copie les coordonnes.
   for (int i = 0; i < (n - 1); i++)
   {
      double x1 = xs[i], x2 = xs[i + 1]
           , y1 = ys[i], y2 = ys[i + 1]
           , m1 = ms[i], m2 = ms[i + 1];
      assert (x2 > x1);

      data.sections[i] = {
         .x1 = x1, .x2 = x2, .y1 = y1, .m1 = m1, .m2 = m2
      };
   }

   return (Interpol) {
      .data.spline = data, .fct = splineFct, .destruct = splineDestruct
   };
}

static double splineFct(InterpolData data, double x)
{
   // Recherche la section par une recherche dicotomique.
   SplineSection *sect = bsearch(
      &x, data.spline.sections, n, sizeof (SplineSection), splineSectionCmp
   );

   assert (section != NULL); // x doit etre dans le domaine de l'interpolation.

   double x1 = section->x1, x2 = section->x2
        , m1 = section->m1, m2 = section->m2;

   double dx = x2 - x1;
   double t = (x - x1) / dx;

   // Calcule les termes recurrents
   double a = t * t; // a = t^2
   double b = a * t; // b = t^3
   double c = 2 * b // c = 2 t^3
        , d = 3 * a // d = 3 t^2
        , e = 2 * a; // e = 2 t^2

   // Calcule les polynomes
   double h00 = c - d + 1, h10 = b - e + t
        , h01 = -c + b   , h11 = b - a;

   return h00 * y1 + h10 * dx * m1 + h01 * y2 + h11 * dx * m2;
}

static double splineDestruct(InterpolData data)
{
   free(data.spline.sections);
}

static int splineSectionCmp(const void *vX, const void *vSection)
{
   double x = *((double *) vX);
   SplineSection section = *((SplineSection *) vSection);

   if (x < section.x1)
      return -1;
   else if (x > section.x2)
      return +1;
   else
      return 0;
}

Interpol lagrangeInterpol(int n, const double xs[], const double ys[])
{
   assert (n >= 1);

   LagrangeData data = {
        .n = n
      , .xs = (double *) malloc(sizeof (double) * n)
      , .ys = (double *) malloc(sizeof (double) * n)
   };
   assert (data.xs != NULL && data.ys != NULL);

   // Copie les coordonnes.
   memcpy(data.xs, xs, sizeof (double) * n);
   memcpy(data.ys, ys, sizeof (double) * n);

   return (Interpol) {
      .data.lagrange = data, .fct = lagrangeFct, .destruct = lagrangeDestruct
   };
}

static double lagrangeFct(InterpolData data, double x)
{
   const double *xs = data.lagrange.xs, *ys = data.lagrange.ys;

   double sum = 0;
   for (int i = 0; i < data.lagrange.n, i++)
   {
      // Calcule la produit des (n-1) polynomes et l'ajoute a la somme
      double product = ys[i]
           , xi = xs[i];
      for (int j = 0; j < data.lagrange.n; i++)
      {
         if (j != i)
         {
            double xj = xs[j];
            product *= (x - xj) / (xi - xj);
         }
      }
      sum += product;
   }

   return sum;
}

static double lagrangeDestruct(InterpolData data)
{
   free(data.lagrange.xs);
   free(data.lagrange.ys);
}
