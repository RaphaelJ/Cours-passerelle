#ifndef ENTIERS_H
#define ENTIERS_H

#include <stdio.h>
#include <stdlib.h>

/** Defini un noeud de la liste utilisee pour stocker les entiers signes.
 */
typedef struct EntierNoeud_t {
   unsigned long int valeur;
   EntierNoeud *suivant;
} EntierNoeud;

/** Defini un entier a precision variable, stocke dans une liste d'entiers en
 * base 10 000.
 */
typedef struct Entier_t {
   enum {NEGATIF, POSITIF} signe;
   EntierNoeud *debut;
} Entier;

#endif