/* Projet de programmation: Gestion d’Images Monochromes
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit quelques transformations et filtres effectuables sur des 
 * images en niveaux de gris.
 */

#ifndef PROCESSING_H
#define PROCESSING_H

#include <stdio.h>
#include <stdlib.h>

#include "pgm.h"

/** Change la luminosite d'une image a l'aide d'une pourcentage.
 * @pre une image en niveaux de gris ainsi qu'un facteur dans [-100; 100].
 *      Un facteur positif éclaircit l'image, un facteur negatif l'assombrit ;
 * @post l'image donnee en argument a subi resultat de l'application du filtre.
 */
void brightness(PGM img, int factor);

/** Effectue une diffuse sur l'image. La valeur de chaque pixel est trouvee
 * en trouvant sa medianne avec ses voisins directs.
 * @pre une image en niveaux de gris et un nombre de passes positif non-nul ;
 * @post l'image donnee en argument a subi resultat de l'application du filtre.
 */
void diffuse(PGM img, int passes);

/** Effectue une mise a l'echelle de l'image.
 * @pre Une image valide et une nouvelle largeur et hauteur positives
 *      non-nulles.
 * @post retourne la nouvelle image. L'image originale n'est pas modifiee.
 */
PGM rescale(PGM img, int w, int h);

#endif
