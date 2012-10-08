/* Projet de programmation: Gestion d’Images Monochromes
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit toutes les operations de bas niveau effectuables sur des
 * images de format PGM (lecture/ecriture depuis/dans un fichier) ainsi que la
 * structure de donnees qui contient ces images.
 */

#ifndef PGM_H
#define PGM_H

#include <stdio.h>
#include <stdlib.h>

/** Contient une image en niveau de gris ainsi que sa taille.
 */
typedef struct {
    int w, h;
    unsigned char *data;
} PGM;

/** Lit une image PGM depuis un fichier texte.
 * @pre Le chemin du fichier donne en argument doit pointer vers une image
 *      PGM valide ;
 * @post L'image lue.
 */
PGM readPgm(const char *path);

/** Ecrit une image PGM dans un fichier texte. Ecrase si le fichier existe.
 * @pre Le chemin du fichier ou ecrire et une image PGM valide.
 */
void writePGM(const char *path, PGM img);

/** Retourne une nouvelle image qui est la copie de l'image donnee en argument.
 * @pre Une image valide ;
 * @post Une copie de cette image.
 */
PGM copyPGM(PGM img);

/** @pre Une image PGM valide dont la memoire doit etre deallouee.
 */
void freePGM(PGM img);

#endif
