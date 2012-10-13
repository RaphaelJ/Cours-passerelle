/* Projet de programmation: Gestion d’Images Monochromes
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit quelques transformations et filtres effectuables sur des
 * images en niveaux de gris.
 */

#include <assert.h>
#include <math.h>

#include "utils.h"
#include "processing.h"

/** Compare deux pixels (utilise pour appeler la fonction qsort()).
 * @pre Deux pointeurs vers deux pixels.
 * @post Retourne un nombre negatif si (le pixel pointe par) a < b, 0 si a = b
 *       ou un nombre positif si a > b.
 */
// static int pixelCmp(const void *a, const void *b);

void brightness(PGM img, int factor)
{
   assert (factor >= -100 && factor <= 100);

   int incr = round(255 * (double) factor / 100);

   int nCells = img.w * img.h;
   for (int i = 0; i < nCells; i++)
   {
      int new_val = (int) img.data[i] + incr;
      img.data[i] = min(255, max(0, new_val));
   }
}

PGM diffuse(PGM img, int passes)
{
   assert (passes > 0);

   PGM res;
   for (int i = 0; i < passes; i++)
   {
      res = allocatePGM(img.w, img.h);

      for (int l = 0; l < img.h; l++)
      {
         // Calcule les indices pour lequels les cellules sont dans l'image
         int minLgn = max(0, l - 1)
            , maxLgn = min(l + 1, img.h - 1);
         for (int c = 0; c < img.w; c++)
         {
            int minCol = max(0, c - 1)
              , maxCol = min(c + 1, img.w - 1);

            /* Itere toutes les cellules de la matrice noyau de diffusion
             * qui sont dans l'image */
            Pixel kernel[9];
            int ik = 0;
            for (int lk = minLgn; lk <= maxLgn; lk++)
               for (int ck = minCol; ck <= maxCol; ck++)
                  kernel[ik++] = img.data[lk * img.w + ck];

            /* Calcule la mediane
             * Effectue un simple tri par selection jusqu'a trouver la ik / 2
             * ieme valeur la plus petite (n'effectue pas un tri complet).
             * Etant donne que la matrice noyau est petite, ce tri s'effectue
             * plus rapidement que le quicksort de la stdlib (l'operation de
             * diffusion s'effectue 2,2 fois plus rapidement qu'avec qsort(), en
             * commentaire plus bas). */
            int nSamples = ik // Nombre de points du noyau dans l'image
              , medianIndex = ik / 2 + ik % 2;
            for (ik = 0; ik <= medianIndex; ik++)
            {
               for (int jk = ik; jk < nSamples; jk++)
               {
                  if (kernel[ik] > kernel[jk])
                  {
                     Pixel tmp = kernel[ik];
                     kernel[ik] = kernel[jk];
                     kernel[jk] = tmp;
                  }
               }
            }

            // qsort(kernel, ik, sizeof (Pixel), pixelCmp);

//             if (nSamples % 2)
               res.data[l * img.w + c] = kernel[medianIndex];
//             else
//             {
//                // Calcule la mediane entre deux valeurs pour les medianes paires
//                int a = kernel[ik / 2]
//                  , b = kernel[medianIndex];
//                res.data[l * img.w + c] = (a + b) / 2;
//             }
         }
      }

      if (i > 0) // Ne libere que les images intermediares
         freePGM(img);
      img = res;
   }

   return res;
}

PGM rescale(PGM img, int w, int h)
{
   assert (w > 0 && h > 0);

   PGM res = allocatePGM(w, h);
   for (int l = 0; l < h; l++)
   {
      int origL = l * img.h / h;
      for (int c = 0; c < w; c++)
      {
         int origC = c * img.w / w;
         res.data[l * w + c] = img.data[origL * img.w + origC];
      }
   }

   return res;
}

/* static int pixelCmp(const void *a, const void *b)
 * {
 *    return *((Pixel *) a) - *((Pixel *) b);
 * } */
