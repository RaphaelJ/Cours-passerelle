/* Projet de programmation: Interpolation Numerique
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit les fonctions pour tracer les interpolations et les
 * courbes de Bezier dans GnuPlot.
 */

#ifndef PLOT_H
#define PLOT_H

#include <stdio.h>
#include <stdlib.h>

#include "point.h"
#include "interpolations.h"

/** Trace m points interpoles entre x1 et x2 a l'aide de GnuPlot dans un
 * graphique nomme title, affiche le resultat et l'enregistre dans outputPath.
 * @pre interpol est un objet genere par l'une des fonction d'interpolation,
 *      x2 > x1 et m >= 1.
 */
void plotInterpol(
     Interpol interpol, char title[], double x1, double x2, int m
   , const char outputPath[]
);

/** Trace m points de la courbe de Bezier definie par les n points de controle
 * ps a l'aide de GnuPlot dans un graphique nomme title, affiche le resultat et
 * l'enregistre dans outputPath.
 * @pre n >= 1 et m >= 1.
 */
void plotBezier(
   int n, const Point ps[], char title[], int m, const char outputPath[]
);

#endif
