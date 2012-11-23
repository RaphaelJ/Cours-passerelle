/* Programming techniques: Polygon
 * Raphael Javaux - November 2012.
 *
 * This file defines a simple but efficient algorithm to draw a simple polygon
 * from an unordered set of points.
 */

// To enable qsort_r():
#define _GNU_SOURCE

#include <assert.h>

#include "polygon.h"

// This function compares two points by their slopes relative to a third point
// (named first) which is a the left of both points.
// This function is used by the qsort_r() sorting function as a comparison
// routine.
static int point_cmp(const void *v_pt1, const void *v_pt2, void* v_first);

void polygon(POINT points[], size_t n)
{
    // According to Wikipedia, a polygon of one point is a point and a polygon
    // of two is a straight line and they are both valid.
    assert (n >= 1);

    // Extracts the leftmost point and swaps it with the first point of the
    // array.
    for (size_t i = 1; i < n; i++) {
        if (points[i].x < points[0].x) {
            POINT tmp = points[i];
            points[i] = points[0];
            points[0] = tmp;
        }
    }

    // Uses the leftmost point as the first point of the polygon.
    // Sort the remainder of the vector by comparing the relative slope of each
    // point to this first point.
    qsort_r(points + 1, n - 1, sizeof (POINT), point_cmp, (void *) points);

    // By selecting the leftmost point, I'm able to compare points by their
    // slopes instead of their angles (because I don't care in which side of the
    // first point they sit).
    // This is way faster because I avoid expensive trigonometric computations.
    // Moreover, as the computation and comparison of two slopes is fast, I
    // don't cache them.
}

static int point_cmp(const void *v_pt1, const void *v_pt2, void* v_first)
{
    POINT pt1 = *(POINT *) v_pt1
        , pt2 = *(POINT *) v_pt2
        , first = *(POINT *) v_first;

    // Computes the coordinates of the two points relative to the first point.
    double dx1 = pt1.x - first.x, dy1 = pt1.y - first.y
         , dx2 = pt2.x - first.x, dy2 = pt2.y - first.y;

    // Compares the slope between two points.
    // Comparing y1 / x1 to y2 / x2 is the same as comparing y1 * x2 to y2 * x1
    // as x1 and x2 are always positive (first is the leftmost point).
    // By replacing divisions by multiplications, we strongly reduce the number
    // of CPU cycles needed by the comparison (from ~90 to ~20 cycles on a
    // new Core processor).
    if (dy1 * dx2 < dy2 * dx1)
        return -1;
    else
        return 1;

    // I don't care about the case when both slopes are the same because the
    // equality test between two floating point numbers after a bunch of
    // arithmetic operations is quite unpredictable.
    // For example, "36.6 + 0.2 == 36.8" gives "false".
    // However, if we were dealing with integers, we will have to compare the
    // distance between our points if the slopes are the same to avoid edge
    // overlap in the final polygon.
}
