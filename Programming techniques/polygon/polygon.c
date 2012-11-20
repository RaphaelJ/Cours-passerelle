/* Programming techniques: Polygon
 * Raphael Javaux - November 2012.
 *
 * This file defines a simple but efficient algorithm to draw a simple polygon
 * from an unordered set of points.
 */

#include <assert.h>

#include "polygon.h"

void polygon(POINT points[], size_t n)
{
    // According to Wikipedia, a polygon of one point is a point and a polygon 
    // of two is a straight line.
    assert (n >= 1);

    // Takes the first point from the vector and uses it as the first point of
    // the polygon. Sort the remainder of the vector by the slope and the
    // distance of the points relative to this first point.
    qsort_r(points + 1, n - 1, point_cmp, (void *) points);
}

int point_cmp(void *v_pt1, void *v_pt2, void* v_first)
{
    POINT pt1 = *v_pt1, pt2 = *v_pt2, first = *v_first;

    // Computes the coordinates of the two points relative to the first point.
    double x1 = pt1.x - first.x, y1 = pt1.y - first.y
         , x2 = pt2.x - first.x, y2 = pt2.y - first.y;

    
}