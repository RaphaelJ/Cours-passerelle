/* Projet de programmation: Interpolation Numerique
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier effectue le parsing des arguments donnes par la ligne de
 * commandes, lit les fichiers des donnes et lance les traces adaptes.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "point.h"
#include "interpolations.h"
#include "plot.h"

/** Affiche un message d'aide detaillant l'utilisation du programme.
 */
static void usage(void);

/** Retourne le nouveau nom de fichier qui va contenir l'image du graphique
 * a partir du nom du fichier de donnees et du nom de la methode
 * d'interpolation.
 * @post <path>-<method>.ps ou path est le nom du fichier de donnes et method
 *       le nom de la methode d'interpolation.
 */
static char *outputPath(const char *path, const char *method);

/** Lit les donnees du fichier path.
 * @post Les points dans ps et les eventuels points avec derivee dans pds
 *       (pds sera un pointer vers NULL si les derivees ne sont pas connues).
 *       Retourne le nombre de points lus.
 */
static int readData(const char *path, Point **ps, PointDer **pds);

int main(int argc, char *argv[])
{
   if (argc < 3)
      usage();
   else
   {
      // Lit les donnees
      Point *ps = NULL;
      PointDer *pds = NULL;
      int n = readData(argv[1], &ps, &pds);

      int nPlot = atoi(argv[2]);

      // Effectue les interpolations
      Interpol interpol;
      char *path;

      if (n >= 2)
      {
         // Trie les points par ordre croissant d'abcisse.
         pointSort(n, ps);

         // Interpolation lineaire
         interpol = linearInterpol(n, ps);
         path = outputPath(argv[1], "lineaire");
         plotInterpol(
            interpol, "Interpolation lineaire", ps[0].x, ps[n-1].x, nPlot, path
         );
         free(path);
         freeInterpol(interpol);

         if (pds != NULL)
         {
            // Interpolation par spline
            interpol = splineInterpol(n, pds);
            path = outputPath(argv[1], "spline");
            plotInterpol(
                 interpol, "Interpolation par spline", ps[0].x, ps[n-1].x, nPlot
               , path
            );
            free(path);
            freeInterpol(interpol);
         }
      }

      // Interpolation de Lagrange
      interpol = lagrangeInterpol(n, ps);
      path = outputPath(argv[1], "lagrange");
      plotInterpol(
         interpol, "Interpolation de Lagrange", ps[0].x, ps[n-1].x, nPlot, path
      );
      free(path);
      freeInterpol(interpol);

      // Courbe de Bezier
      path = outputPath(argv[1], "bezier");
      plotBezier(n, ps, "Courbe de Bezier", nPlot, path);
      free(path);

      free(ps);
      free(pds); // free(NULL) ne fait rien
   }

   return 0;
}

static void usage(void)
{
   printf(
      "Trace l'interpolation d'une fonction dont les valeurs se trouvent\n"
      "dans un fichier de donnees.\n"
      "USAGE: ./main <fichier de donnees> <nombre de points>\n"
   );
}

static char *outputPath(const char *path, const char *method)
{
   size_t len = strlen(path);

   // Compose le nouveau nom du fichier
   size_t lenMethod = strlen(method);
   char *new = malloc(sizeof (char) * (len + lenMethod + 6));
   assert (new);
   sprintf(new, "%s-%s.png", path, method);

   return new;
}

static int readData(const char *path, Point **ps, PointDer **pds)
{
   FILE *f = fopen(path, "r");
   assert (f);

   char line[255];

   // Lit le nombre de points.
   int n;
   fgets(line, 255, f);
   sscanf(line, "%d", &n);

   *ps = (Point *) malloc(sizeof (Point) * n);
   assert (*ps);

   // Lit le premier point pour verifier si les tengentes sont fournies.
   PointDer pd;
   fgets(line, 255, f);
   int nElems = sscanf(line, "%lf %lf %lf", &pd.x, &pd.y, &pd.m);

   (*ps)[0].x = pd.x;
   (*ps)[0].y = pd.y;

   if (nElems == 3) // Lit les tangentes et les coordonnes
   {
      *pds = (PointDer *) malloc(sizeof (PointDer) * n);
      assert (*pds);

      (*pds)[0] = pd;

      for (int i = 1; i < n; i++)
      {
         fgets(line, 255, f);
         sscanf(line, "%lf %lf %lf", &(*pds)[i].x, &(*pds)[i].y, &(*pds)[i].m);

         (*ps)[i].x = (*pds)[i].x;
         (*ps)[i].y = (*pds)[i].y;
      }
   }
   else // Lit uniquement les coordonnes
   {
      (*pds) = NULL;

      for (int i = 1; i < n; i++)
      {
         fgets(line, 255, f);
         sscanf(line, "%lf %lf", &(*ps)[i].x, &(*ps)[i].y);
      }
   }

   fclose(f);

   return n;
}
