/* Projet de programmation: Interpolation Numerique
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier effectue le parsing des arguments donnes par la ligne de
 * commandes et effectue les transformations appropriees.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <assert.h>

#include "pgm.h"
#include "processing.h"

/** Affiche un message d'aide detaillant l'utilisation du programme.
 */
static void usage(void);

/** Retourne le nouveau nom de fichier auquel filterName a ete rajoute.
 * @pre path se termine par l'extension .pgm ;
 * @post basepath-<filterName>.pgm ou basepath est le chemin donne en entree
 *       ou l'extension .pgm a ete retiree.
 */
static char *newImagePath(const char *path, const char *filterName);

int main(int argc, char *argv[])
{
   if (argc <= 2)
      usage();
   else
   {
      PGM img = readPGM(argv[1]);

      if (argc == 3 && strcmp(argv[2], "recopie") == 0)
      {
         char *newPath = newImagePath(argv[1], "recopie");
         writePGM(newPath, img);
         free(newPath);
      }
      else if (argc == 4 && strcmp(argv[2], "luminosite") == 0)
      {
         char *newPath = newImagePath(argv[1], "luminosite");
         brightness(img, atoi(argv[3]));
         writePGM(newPath, img);
         free(newPath);
      }
      else if (argc == 4 && strcmp(argv[2], "diffusion") == 0)
      {
         char *newPath = newImagePath(argv[1], "diffusion");
         PGM newImg = diffuse(img, atoi(argv[3]));
         writePGM(newPath, newImg);
         free(newPath);
         freePGM(newImg);
      }
      else if (argc == 5 && strcmp(argv[2], "echelle") == 0)
      {
         char *newPath = newImagePath(argv[1], "echelle");
         PGM newImg = rescale(img, atoi(argv[4]), atoi(argv[3]));
         writePGM(newPath, newImg);
         free(newPath);
         freePGM(newImg);
      }
      else
         usage();

      freePGM(img);
   }

   return 0;
}

static void usage(void)
{
   printf(
      "Applique des filtres simples sur des images au format PGM.\n"
      "USAGE: ./main <image> recopie\n"
      "       ./main <image> luminosite <facteur>\n"
      "       ./main <image> diffusion <passes>\n"
      "       ./main <image> echelle <hauteur> <largeur>\n"
   );
}

static char *newImagePath(const char *path, const char *filterName)
{
   // Verifie que le nom de fichier se termine par .pgm.
   size_t len = strlen(path);
   assert (strcmp(path + (len - 4), ".pgm") == 0);

   // Compose le nouveau nom du fichier
   size_t lenFilter = strlen(filterName);
   char *new = malloc(sizeof (char) * (len + lenFilter + 2));
   strncpy(new, path, len - 4);
   new[len - 4] = '-';
   strcpy(new + len - 3, filterName);
   strcpy(new + len - 3 + lenFilter, ".pgm");

   return new;
}
