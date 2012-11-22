/* Programming techniques: Polygon
 * Raphael Javaux - November 2012.
 *
 * This file defines a simple but efficient algorithm to draw a simple polygon
 * from an unordered set of points.
 */

#define _GNU_SOURCE

#include <assert.h>

#include "polygon.h"

// This function compares two points by their slopes relative to a third point
// (named first).
// This function is used by the qsort_r() sorting function as a comparison
// routine.
int point_cmp(const void *v_pt1, const void *v_pt2, void* v_first);

void polygon(POINT points[], size_t n)
{
    // According to Wikipedia, a polygon of one point is a point and a polygon
    // of two is a straight line.
    assert (n >= 1);

    // Takes the first point from the vector and uses it as the first point of
    // the polygon. Sort the remainder of the vector by comparing the relative
    // slope of each point to this first point.
    qsort_r(points + 1, n - 1, sizeof (POINT), point_cmp, (void *) points);
}

int point_cmp(const void *v_pt1, const void *v_pt2, void* v_first)
{
    POINT pt1 = *(POINT *) v_pt1
        , pt2 = *(POINT *) v_pt2
        , first = *(POINT *) v_first;

    // Computes the coordinates of the two points relative to the first point.
    double x1 = pt1.x - first.x, y1 = pt1.y - first.y
         , x2 = pt2.x - first.x, y2 = pt2.y - first.y;

    // Compares the slope between two points.
    // Comparing y1 / x1 to y2 / x2 is the same as comparing y1 * x2 to y2 * x1.
    // By replacing divisions by multiplications, we strongly reduce the number
    // of CPU cycles needed by the comparison (from ~90 to ~20 cycles on a
    // new Core processor).
    if (y1 * x2 < y2 * x1)
//     if (y1 / x1 < y2 * x1)
        return -1;
    else
        return 1;

    // I don't care about the case when both slopes are the same because the
    // equality test between two floating point numbers after a bunch of
    // arithmetic operations is unpredictable.
    // For example, "36.6 + 0.2 == 36.8" gives "false".
    // However, if we were dealing with integers, we will have to compare the
    // distance between our points if the slopes are the same to avoid edge
    // overlap in the final polygon.

    // We can go deeper in optimisation by replacing the conditional expression
    // by this expression :
    double diff = y1 * x2 - y2 * x1;
    return (int) *((long long *) &diff);
    //
    // This is easy to understand if you know that :
    //    1. the subtraction between two numbers will return a negative number
    //       if the first is smaller than the second and a positive number if
    //       the second is smaller ;
    //    2. an integer representation of a negative/positive floating point
    //       number is still a negative/positive number as long as the bit sign
    //       is on the same position on both representations ;
    //    3. *((long long *) &var) is just a way to change the type of var
    //       WITHOUT casting its value (it's a free operation as the compiler
    //       outputs no instruction for this).
    //
    // This reduces the number of cycles needed by the conditional expression by
    // half on a modern processor, in addition to remove potential branch
    // miss-predictions. But this "hack" only works if the sign bit is on the
    // same position on both types, which is not defined in the C language
    // specification.
}
