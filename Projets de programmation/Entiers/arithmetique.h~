#ifndef ARITHMETIQUE_H
#define ARITHMETIQUE_H

#include <stdio.h>
#include <stdlib.h>

#include <assert.h>
#include <stdbool.h>

#include "entiers.h"

/** Defini un noeud de la liste utilisee pour stocker les entiers signes.
 * Les groupes de 4 chiffres les plus significatifs sont places sur les premiers 
 * noeuds du nombre.
 */
typedef struct EntierNoeud_t {
   int valeur;
   struct EntierNoeud_t *suivant;
} EntierNoeud;

/** Defini un entier a precision variable, stocke dans une liste d'entiers en
 * base 10 000.
 */
typedef struct Entier_t {
   enum { NEGATIF, POSITIF } signe;
   EntierNoeud *debut;
} Entier;

Entier creerEntier(char* chaine);
void libererEntier(Entier entier);

int nul(Entier entier);
int positif(Entier entier);

void afficherEntier(Entier entier);

#endif
