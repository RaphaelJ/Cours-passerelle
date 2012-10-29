/* Projet de programmation: Interpolation Numerique
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit les algorithmes et les structures pour tracer une courbe
 * de Bezier.
 */

#ifndef BEZIER_H
#define BEZIER_H

#include <stdio.h>
#include <stdlib.h>

#include "point.h"

/** Trace m points d'une courbe de Bezier a partir d'un ensemble ps de n points.
 * @pre Un tableau ps de n points (au moins un point) ;
 * @post le vecteur des m points de la courbe de Bezier representee par les 
 *       points de controle ps.
 */
Point *bezierCurve(int n, const Point ps[], int m);

#endif
