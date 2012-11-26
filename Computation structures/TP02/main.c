/* Programming techniques: Polygon
 * Raphael Javaux - November 2012.
 *
 * This file read a list of points from stdin and output the corresponding
 * simple polygon on stdin.
 */

#include <stdio.h>
#include <stdlib.h>

#include <assert.h>

#include "polygon.h"

int main(void)
{
    // Reads the number of points given as input.
    int n;
    scanf("%d", &n);

    // Reads and stores the points.
    POINT *points = (POINT *) malloc(sizeof (POINT) * n);
    for (int i = 0; i < n; i++)
        scanf("%lf %lf", &points[i].x, &points[i].y);

    // Sorts the polygon and write the result to stdout.
    polygon(points, n);
    printf("%d\n", n + 1);
    for (int i = 0; i < n; i++)
        printf("%lf %lf\n", points[i].x, points[i].y);
    printf("%lf %lf\n", points[0].x, points[0].y);

    free(points);

    return 0;
}
