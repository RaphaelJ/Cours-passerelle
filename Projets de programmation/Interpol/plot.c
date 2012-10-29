/* Projet de programmation: Interpolation Numerique
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit les fonctions pour tracer les interpolations et les
 * courbes de Bezier dans GnuPlot.
 */

#include <assert.h>
#include <string.h>

#include "gnuplot_i.h"

#include "bezier.h"

#include "plot.h"

/** Initialise et retourne un contexte GnuPlot ou les axes ont ete initialises.
 */
static gnuplot_ctrl *initPlot(void);

/** Affiche et enregistre le graphique des n points (xs, ys) avec un title comme
 * titre.
 * @pre h est un handler GnuPlot. outputPath est le nom de fichier ou sera
 *      enregistre le graphique ;
 */
static void showAndSaveGraph(
   int n, double *xs, double *ys, char title[], const char outputPath[]
);

void plotInterpol(
     Interpol interpol, char title[], double x1, double x2, int m
   , const char outputPath[]
)
{
   assert (x2 > x1);
   assert (m >= 1);

   // Calcule les interpolations dans les vecteurs xs et ys.
   double dx = (x2 - x1) / m;
   double *xs = (double *) malloc(sizeof (double) * m);
   double *ys = (double *) malloc(sizeof (double) * m);
   assert (xs != NULL && ys != NULL);

   for (int i = 0; i < m; i++)
   {
      double x = x1 + dx * i;

      xs[i] = x;
      ys[i] = interpolate(interpol, x);
   }

   showAndSaveGraph(m, xs, ys, title, outputPath);

   free(xs);
   free(ys);
}

void plotBezier(
   int n, const Point ps[], char title[], int m, const char outputPath[]
)
{
   assert (n >= 1);
   assert (m >= 1);

   Point *curve = bezierCurve(n, ps, m);

   // Copie les points de la courbes dans les vecteurs pour GnuPlot.
   double *xs = (double *) malloc(sizeof (double) * m);
   double *ys = (double *) malloc(sizeof (double) * m);
   assert (xs != NULL && ys != NULL);

   for (int i = 0; i < m; i++)
   {
      xs[i] = curve[i].x;
      ys[i] = curve[i].y;
   }

   free(curve);

   showAndSaveGraph(m, xs, ys, title, outputPath);

   free(xs);
   free(ys);
}

static gnuplot_ctrl *initPlot(void)
{
   // Initialise GnuPlot
   gnuplot_ctrl *h = gnuplot_init();

   // Configure les axes et le trace.
   gnuplot_setstyle(h, "linespoints");
   gnuplot_set_xlabel(h, "X");
   gnuplot_set_ylabel(h, "Y");

   return h;
}

static void showAndSaveGraph(
   int n, double *xs, double *ys, char title[], const char outputPath[]
)
{
   // Affiche le graphique.
   gnuplot_ctrl *h = initPlot();

   gnuplot_plot_xy(h, xs, ys, n, title);

   printf("Affichage du graphique %s ...\n", title);
   printf("Appuyez sur une touche pour fermer la fenetre.\n");
   getchar();

   gnuplot_close(h);


   // Enregistre le graphique
   h = initPlot();

   gnuplot_cmd(h, "set terminal png");

   // Specifie le fichier ou sauvegarder le graphique.
   char *saveCmd = (char *) malloc(sizeof (char) * strlen(outputPath) + 14);
   sprintf(saveCmd, "set output \"%s\"", outputPath);
   gnuplot_cmd(h, saveCmd);
   free(saveCmd);

   gnuplot_plot_xy(h, xs, ys, n, title);

   printf("Le graphique a ete enregistre dans le fichier %s.\n", outputPath);

   gnuplot_close(h);
}
