/* Projet de programmation: Interpolation Numerique
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit les algorithmes et les structures permettant d'obtenir les
 * fonctions d'interpolations.
 */

#include <assert.h>
#include <string.h>

#include "interpolations.h"

/** @pre Les donnees d'interpolation lineaire et un point dans le domaine
 *       d'interpolation (X dans [X1, Xn]) ;
 * @post le point a l'ordonnee correspondante estimee.
 */
static double linearFct(InterpolData data, double x);

/** @pre Les donnees de l'interpolation lineaire a liberer.
 */
static void linearDestruct(InterpolData data);

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
 *       d'interpolation (X dans [X1, Xn]) ;
 * @post le point a l'ordonnee correspondante estimee.
 */
static double splineFct(InterpolData data, double x);

/** @pre Les donnees de l'interpolation par spline a liberer.
 */
static void splineDestruct(InterpolData data);

/** Comparateur utilise pour la recherche dicotomique de la section d'une
 * interpolation par spline.
 * @pre Un pointeur vers une abscisse et une section de l'interpolation
 *      par spline ;
 * @post un nombre negatif si l'abcsisse est avant le domaine de la section,
 *       un nombre nul si elle est dans le domaine et un nombre positif si
 *       elle est apres le domaine de la section.
 */
static int splineSectionCmp(const void *vX, const void *vSection);


/** @pre Les donnees d'interpolation de Lagrange et l'abscisse d'un point a 
 *       interpoler ;
 * @post le point a l'ordonnee correspondante estimee.
 */
static double lagrangeFct(InterpolData data, double x);

/** @pre Les donnees de l'interpolation de Lagrange a liberer.
 */
static void lagrangeDestruct(InterpolData data);

double interpolate(Interpol interpol, double x)
{
   return interpol.fct(interpol.data, x);
}

void freeInterpol(Interpol interpol)
{
   interpol.destruct(interpol.data);
}

Interpol linearInterpol(int n, const Point ps[])
{
   assert (n >= 2);

   LinearData data = {
        .n = n - 1
      , .sections = (LinearSection *) malloc(sizeof (LinearSection) * (n - 1))
   };
   assert (data.sections != NULL);

   // Calcule la pente de chaque section et copie les coordonnes.
   for (int i = 0; i < data.n; i++)
   {
      Point p1 = ps[i]
          , p2 = ps[i + 1];
      assert (p2.x > p1.x);

      data.sections[i] = (LinearSection) {
         .x1 = p1.x, .x2 = p2.x, .y1 = p1.y, .m = (p2.y - p1.y) / (p2.x - p1.x)
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
        &x, data.linear.sections, data.linear.n, sizeof (LinearSection)
      , linearSectionCmp
   );

   assert (section != NULL); // x doit etre dans le domaine de l'interpolation.

   return section->m * (x - section->x1) + section->y1;
}

static void linearDestruct(InterpolData data)
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

Interpol splineInterpol(int n, const PointDer ps[])
{
   assert (n >= 2);

   SplineData data = {
        .n = n - 1
      , .sections = (SplineSection *) malloc(sizeof (SplineSection) * (n - 1))
   };
   assert (data.sections != NULL);

   // Copie les coordonnes.
   for (int i = 0; i < data.n; i++)
   {
      PointDer p1 = ps[i]
             , p2 = ps[i + 1];
      assert (p2.x > p1.x);

      data.sections[i] = (SplineSection) {
         .p1 = p1, .p2 = p2
      };
   }

   return (Interpol) {
      .data.spline = data, .fct = splineFct, .destruct = splineDestruct
   };
}

static double splineFct(InterpolData data, double x)
{
   // Recherche la section par une recherche dicotomique.
   SplineSection *section = bsearch(
        &x, data.spline.sections, data.spline.n, sizeof (SplineSection)
      , splineSectionCmp
   );

   assert (section != NULL); // x doit etre dans le domaine de l'interpolation.

   PointDer p1 = section->p1
          , p2 = section->p2;

   double dx = p2.x - p1.x;
   double t = (x - p1.x) / dx;

   // Calcule les termes recurrents
   double a = t * t; // a = t^2
   double b = a * t; // b = t^3
   double c = 2 * b  // c = 2 t^3
        , d = 3 * a  // d = 3 t^2
        , e = 2 * a; // e = 2 t^2

   // Calcule les polynomes
   double h00 = c - d + 1, h10 = b - e + t
        , h01 = -c + d   , h11 = b - a;

   return h00 * p1.y + h10 * dx * p1.m + h01 * p2.y + h11 * dx * p2.m;
}

static void splineDestruct(InterpolData data)
{
   free(data.spline.sections);
}

static int splineSectionCmp(const void *vX, const void *vSection)
{
   double x = *((double *) vX);
   SplineSection section = *((SplineSection *) vSection);

   if (x < section.p1.x)
      return -1;
   else if (x > section.p2.x)
      return +1;
   else
      return 0;
}


Interpol lagrangeInterpol(int n, const Point ps[])
{
   assert (n >= 1);

   LagrangeData data = {
        .n = n, .ps = (Point *) malloc(sizeof (Point) * n)
   };
   assert (data.ps != NULL);

   // Copie les coordonnes localement.
   memcpy(data.ps, ps, sizeof (Point) * n);

   return (Interpol) {
      .data.lagrange = data, .fct = lagrangeFct, .destruct = lagrangeDestruct
   };
}

static double lagrangeFct(InterpolData data, double x)
{
   const Point *ps = data.lagrange.ps;


   double sum = 0;
   for (int i = 0; i < data.lagrange.n; i++)
   {
      // Calcule la produit des (n-1) polynomes et l'ajoute a la somme
      double product = ps[i].y
           , xi = ps[i].x;
      for (int j = 0; j < data.lagrange.n; j++)
      {
         if (j != i)
         {
            double xj = ps[j].x;
            product *= (x - xj) / (xi - xj);
         }
      }
      sum += product;
   }

   return sum;
}

static void lagrangeDestruct(InterpolData data)
{
   free(data.lagrange.ps);
}
