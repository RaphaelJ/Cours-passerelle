/* Programming techniques: Polygon
 * Raphael Javaux - Novembre 2012.
 *
 * This file defines a simple but efficient algorithm to draw a simple polygon
 * from an unordered set of points.
 */

#ifndef POLYGON_H
#define POLYGON_H

#include <stdio.h>
#include <stdlib.h>

// Structure which contains the coordinates of a point.
typedef struct {
    double x, y;
} POINT;

// Changes the order of the points in the input array so the array can be used
// to draw a simple, not crossing, polygon.
// n is the size of the input array and must be greater or equals to 1.
void polygon(POINT points[], size_t n);

#endif
