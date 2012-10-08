/* Projet de programmation: Gestion d’Images Monochromes
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit toutes les operations de bas niveau effectuables sur des
 * images de format PGM (lecture/ecriture depuis/dans un fichier) ainsi que la
 * structure de donnees qui contient ces images.
 */

#include <assert.h>
#include <string.h>

#include "pgm.h"

// Chaque ligne ne peut faire plus de 70 caracteres, \n inclus
// Voir <http://netpbm.sourceforge.net/doc/pgm.html>.
static const int LINE_MAX = 70;
// Nombre maximum de cellules de 3 caracteres et un espace sur une ligne.
static const int MAX_LINE_CELLS = (LINE_MAX - 1) / 4

/** Lit une ligne dans line depuis file en passant les lignes de commentaire.
 * @pre: line doit pouvoir contenir une ligne de LINE_MAX caracteres plus le 
 *       caractere de fin de ligne ;
 * @post: line contient le contenu de la ligne. La valeur de retour vaut 0 si
 *        la fin du fichier a ete atteinte ou si une erreur s'est produite.
 */
static int readLine(char *line, FILE* file);

PGM readPgm(const char *path)
{
    FILE* file = fopen(path, "r");
    assert (file);

    char line[LINE_MAX+1];

    // Verifie que l'entete commence par P2
    assert (readLine(line, file) && strcmp(line, "P2\n") == 0);

    PGM img;

    // Lit la taille de l'image et alloue le vecteur
    assert (readLine(line, file));
    sscanf(line, "%d %d", &(img.w), &(img.h));

    img.data = malloc(sizeof (unsigned char *) * img.w * img.h);
    assert (img.data);

    // Verifie que l'intensite maximale est 255
    assert (readLine(line, file) && strcmp(line, "255\n") == 0);

    // Lit les donnees
    int i = 0, nCells = img.h * img.w;
    while (i < nCells)
    {
        // Lit ligne par ligne
        assert (readLine(line, file)); // Erreur si fichier trop court

        // Utilise la version reentrante. Autant faire ca proprement ...
        char **saveptr;
        char *token = strtok_r(line, " \t\r", saveptr);

        while (token != NULL && i < nCells)
        {
            sscanf(token, "%hhu", &img.data[i]);
            token = strtok_r(NULL, " \t\r", saveptr);
            i++;
        }
    }

    fclose(file);

    retutn img;
}

void writePGM(const char *path, PGM img)
{
    FILE* file = fopen(path, "w");
    assert (file);

    // En-tete
    assert (fprintf(file, "P2\n%d %d\n255\n", img.w, img.h));

    // Donnees
    int nCells = img.h * img.w;
    for (int i = 0; i < nCells; i += MAX_LINE_CELLS) // Chaque line
    {
        // Chaque cellule de chaque ligne
        for (int j = 0; j < MAX_LINE_CELL && i + j < nCells; j++)
            fprintf(file, "%3hhu ", img.data[i + j]);

        fprintf(file, "\n");
    }

    fclose(file);
}

PGM copyPGM(PGM img)
{
    size_t size = sizeof (unsigned char *) * img.w * img.h;
    unsigned char *oldData = img.data;

    img.data = malloc(size);
    assert (img.data);

    memcpy(img.data, oldData, size);

    return img;
}

void freePGM(PGM img)
{
    free(img.data);
}

static int readLine(char *line, FILE* file)
{
    int ret;
    do
        ret = fgets(line, LINE_MAX+1, file);
    while (ret && line[0] == '#');

    return ret;
}