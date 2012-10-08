/* Projet de programmation: Gestion d’Images Monochromes
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit quelques transformations et filtres effectuables sur des 
 * images en niveaux de gris.
 */

#include <assert.h>

#include "processing.h"

void brightness(PGM img, int factor)
{
    assert (factor >= )
}

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