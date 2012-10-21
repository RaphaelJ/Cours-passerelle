/* Projet de programmation: Interpolation Numerique
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit les algorithmes et les structures permettant d'obtenir les
 * fonctions d'interpolations.
 */

#ifndef INTERPOLATIONS_H
#define INTERPOLATIONS_H

#include <stdio.h>
#include <stdlib.h>

/** Contient les informations d'une section de l'interpolation lineaire. */
typedef struct {
   double x1, x2;
   double y1;
   double m; // Pente
} LinearSection;

/** Contient les donnees utilisees lors d'une interpolation lineaire. */
typedef struct {
   int n; // Nombre de sections
   LinearSection *sections;
} LinearData;

/** Contient les informations d'une section de l'interpolation par splines. */
typedef struct {
   double x1, x2, y1, y2;
   double m1, m2; // Derivee locale
} SplineSection;

/** Contient les donnees utilisees lors d'une interpolation par splines. */
typedef struct {
   int n; // Nombre de sections
   SplineSection *sections;
} SplineData;

/** Contient les donnees utilisees lors d'une interpolation par un polynome de 
 * Lagrange. */
typedef struct {
   int n; // Nombre de points
   double *xs, *ys;
//    double denom; // Produit des denominateurs des polynomes de Lagrange
} LagrangeData;

/** Contient les donnees utilisees lors d'une interpolation par des courbes de 
 * Bezier. */
typedef struct {
   int n; // Nombre de points
   double *xs, *ys;
} BezierData;

/** Contient les donnees passees aux differentes methodes de calcul des
 * interpolations.
 * Remarque: l'union va allouer legerement plus de place que necessaire dans les
 *           cas des interpolations lineaires et par splines, mais cela evite
 *           d'avoir a allouer ces structures sur le tas, ce qui au final
 *           devrait etre largement plus rapide.
 */
typedef union {
   LinearSection linear;
   SplineData spline;
   LagrangeData lagrange;
   BezierData bezier;
} InterpolData;

/** Structure retournee par les fonctions realisant les interpolations.
 * Les donnees issues de l'interpolation sont passees a la fonction qui
 * retourne l'ordonnee du point d'abscisse donnee.
 * Remarque: en quelque sorte, le pointeur de fonction fct simule grossierement
 *           une methode virtuelle dans un langage oriente objets, ou une
 *           closure dans un langage fonctionnel, et destruct joue le role d'un
 *           destructeur.
 */
typedef struct {
   InterpolData data;

   /** Pointeur de fonction qui, etant donne les donnees d'interpolation,
    * retourne le point d'ordonnee interpolee sur l'abscisse donnee.
    * @pre Les donnees correspondant a la methode d'interpolation et un point
    *      d'abscisse ;
    * @post le point a l'ordonnee correspondante estimee.
    */
   double (*fct)(InterpolData data, double x);

   /** Utilise pour liberer la memoire utilisee par les donnees de 
    * l'interpolation.
    * @pre Les donnees correspondant a la methode d'interpolation.
    */
   double (*destruct)(InterpolData data);
} Interpol;

/** Retourne l'ordonnee du point donne en argument en utilisant la methode et
 * les donnees de la structure interpol.
 */
double interpolate(Interpol interpol, double x);

/** Libere les donnees en utilisant le destructeur approprie a la methode
 * d'interpolation.
 */
void freeInterpol(Interpol interpol);

/** @pre Le nombre et les coordonnees des points a utiliser durant les
 *       interpolations (au moins deux points). Les points doivent etre donnes 
 *       en abscisse croissante ;
 * @post les donnees a utiliser pour interpoler un point.
 */
Interpol linearInterpol(int n, const double xs[], const double ys[]);

/** @pre Le nombre, les coordonnees et les tangentes des points a utiliser
 *       durant les interpolations (au moins deux points). Les points doivent
 *       etre donnes en abscisse croissante ;
 * @post les donnees a utiliser pour interpoler un point.
 */
Interpol splineInterpol(
   int n, const double xs[], const double ys[], const double ms[]
);

Interpol lagrangeInterpol(int n, const double xs[], const double ys[]);
Interpol bezierInterpol(int n, const double xs[], const double ys[]);

#endif
