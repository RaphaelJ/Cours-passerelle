/* Projet de programmation: Gestion d’Images Monochromes
 * Raphael Javaux - Octobre 2012.
 *
 * Ce fichier definit toutes les operations de bas niveau effectuables sur des
 * images de format PGM (lecture/ecriture depuis/dans un fichier) ainsi que la
 * structure de donnees qui contient ces images.
 */

#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include "pgm.h"

// Chaque ligne ne peut faire plus de 70 caracteres, \n inclus
// Voir <http://netpbm.sourceforge.net/doc/pgm.html>.
static const int LINE_MAX = 70;
// Nombre maximum de cellules de 3 caracteres et un espace sur une ligne.
static const int MAX_LINE_CELLS = (LINE_MAX - 1) / 4;

/** Lit une ligne dans line depuis file en passant les lignes de commentaire.
 * @pre line doit pouvoir contenir une ligne de LINE_MAX caracteres plus le 
 *      caractere de fin de ligne ;
 * @post line contient le contenu de la ligne. La valeur de retour vaut NULL si
 *       la fin du fichier a ete atteinte ou si une erreur s'est produite.
 */
static char *readLine(char *line, FILE* file);

/** @pre Deux chaines terminees par un \0 ;
 * @post true si str commence par prefix, false sinon.
 */
static bool isPrefix(const char *prefix, const char *str);

PGM allocatePGM(int w, int h)
{
    assert (w > 0 && h > 0);

    Pixel* data = malloc (sizeof (Pixel) * w * h);
    assert (data);

    return (PGM) {
          .w = w, .h = h
        , .data = data
    };
}

PGM readPGM(const char *path)
{
    FILE* file = fopen(path, "r");
    assert (file);

    char line[LINE_MAX+1];

    // Verifie que l'entete commence par P2
    assert (readLine(line, file) && isPrefix("P2", line));

    // Lit la taille de l'image et alloue le vecteur
    int w, h;
    assert (readLine(line, file));
    sscanf(line, "%d %d", &w, &h);
    PGM img = allocatePGM(w, h);

    // Verifie que l'intensite maximale est 255
    assert (readLine(line, file) && line[0]  strcmp(line, "255\n") == 0);

    // Lit les donnees
    int i = 0, nCells = img.h * img.w;
    while (i < nCells)
    {
        // Lit ligne par ligne
        assert (readLine(line, file)); // Erreur si fichier trop court

        char *token = strtok(line, " \t\r");
        while (token != NULL && i < nCells)
        {
            sscanf(token, "%hhu", &img.data[i]);
            token = strtok(NULL, " \t\r");
            i++;
        }
    }

    fclose(file);

    return img;
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
        for (int j = 0; j < MAX_LINE_CELLS && i + j < nCells; j++)
            fprintf(file, "%3hhu ", img.data[i + j]);

        fprintf(file, "\n");
    }

    fclose(file);
}

PGM copyPGM(PGM img)
{
    PGM newImg = allocatePGM(img.w, img.h);

    memcpy(newImg.data, img.data, sizeof (Pixel) * img.w * img.h);

    return newImg;
}

void freePGM(PGM img)
{
    free(img.data);
}

static char *readLine(char *line, FILE* file)
{
    char *ret;
    do
        ret = fgets(line, LINE_MAX+1, file);
    while (ret && line[0] == '#');

    return ret;
}

static bool isPrefix(const char *prefix, const char *str)
{
    while (*prefix != '\0')
    {
        if (*(prefix++) != *(str++))
            return false;
    }
    return true;
}