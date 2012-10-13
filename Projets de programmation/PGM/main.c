/* Projet de programmation: Gestion d’Images Monochromes
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier effectue le parsing des arguments donnes par la ligne de
 * commandes et effectue les transformations appropriees.
 */

#include <stdio.h>
#include <stdlib.h>

#include <assert.h>

#include "pgm.h"
#include "processing.h"

int main(void)
{
    PGM p = readPGM("/home/rapha/passerelle/Projets de programmation/PGM/grayscale-donnees/saturn.pgm");
    writePGM("out.pgm", p);

    return 0;
}
