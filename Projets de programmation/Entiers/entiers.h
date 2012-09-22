#ifndef ENTIERS_H
#define ENTIERS_H

#include <stdio.h>
#include <stdlib.h>

#include <assert.h>
#include <stdbool.h>

/** Defini un noeud de la liste utilisee pour stocker les entiers signes.
 * Les groupes de 4 chiffres les plus significatifs sont places sur les premiers 
 * noeuds du nombre.
 */
typedef struct EntierNoeud_t {
   int valeur;
   struct EntierNoeud_t *suivant;
} EntierNoeud;

typedef int Signe;

extern const Signe POSITIF;
extern const Signe NEGATIF;

/** Defini un entier a precision variable, stocke dans une liste d'entiers en
 * base 10 000.
 */
typedef struct Entier_t {
   Signe signe;
   EntierNoeud *debut;
} Entier;

/** Accepte une chaine de caracteres en entree et retourne l'entier en precision
 * variable correspondant.
 * @pre Une chaine doit etre composee d'un ou plusieurs caracteres numeriques et
 *      peut debuter avec le caractere "-".
 * @post La representation de l'entier sous une forme a precision variable.
 */
Entier lireEntier(char* chaine);

/** Initialise la structure contenant un entier et son signe.
 * @pre Le signe (POSITIF ou NEGATIF) et le pointeur vers le premier noeud.
 * @post La representation de l'entier sous une forme a precision variable.
 */
Entier creerEntier(Signe signe, EntierNoeud *debut);

/** @post La representation de l'entier 0 sous une forme a precision variable.
 */
Entier zero(void);

/** Alloue un nouveau noeud d'un entier a precision variable.
 * @pre La valeur entiere du noeud et un pointeur vers le noeud suivant.
 * @post Un pointeur vers le nouveau noeud. Une assertion est declenchee si le
 *       noeud n'a pu etre alloue.
 */
EntierNoeud *creerNoeud(int valeur, EntierNoeud *suivant);

/** @pre Un entier valide dont la memoire allouee doit etre liberee
 */
void libererEntier(Entier entier);

/** @pre Un entier valide.
 *  @post Un entier non nul si l'entier est nul.
 */
bool nul(Entier entier);

/** @pre Un entier valide.
 *  @post Un entier non nul si l'entier est positif et non nul.
 */
int positif(Entier entier);

/** Affiche un entier a precision variable sous forme decimale.
 * @pre Un entier a precision variable valide.
 */
void afficherEntier(Entier entier);

#endif
