/* Projet de programmation: Calculs sur des entiers en precision arbitraire.
 * Raphael Javaux - Septembre 2012.
 *
 * Ce fichier contient les fonctions permettant d'initialiser, d'afficher et de
 * liberer les entiers en precision variable.
 * Ce fichier contient egalement les structures de donnees utilisees pour
 * definir ces entiers.
 */

#ifndef ENTIERS_H
#define ENTIERS_H

#include <stdio.h>
#include <stdlib.h>

#include <stdbool.h>

/** Type utilise pour enregistrer les valeurs des noeuds.
 * Le type doit pouvoir stocker les valeurs dans [0; +9999].
 */
typedef unsigned short ValueNode;

/** Type utilise pour effectuer les operations mathematiques sur les valeurs des
 * noeuds.
 * Le type doit pouvoir stocker les valeurs dans [0; 9999^2].
 */
typedef unsigned long int ValueArithm;

/** Defini un noeud de la liste utilisee pour stocker les entiers signes.
 * Les groupes de 4 chiffres les plus significatifs sont places sur les derniers
 * noeuds du nombre.
 */
typedef struct IntegerNode_t {
   ValueNode value;
   struct IntegerNode_t *next;
} IntegerNode;

typedef char Sign;
extern const Sign POSITIVE;
extern const Sign NEGATIVE;

/** Defini un entier a precision variable, stocke dans une liste d'entiers en
 * base 10 000.
 */
typedef struct Integer_t {
   Sign sign;
   IntegerNode *start;
} Integer;

/** Accepte une chaine de caracteres en entree et retourne l'entier en precision
 * variable correspondant.
 * @pre Une chaine doit etre composee d'un ou plusieurs caracteres numeriques et
 *      peut debuter avec le caractere "-" ;
 * @post La representation de l'entier sous une forme a precision variable.
 */
Integer readInteger(char* str);

/** Initialise la structure contenant un entier et son signe.
 * @pre Le signe (POSITIVE ou NEGATIVE) et le pointeur vers le premier noeud ;
 * @post La representation de l'entier sous une forme a precision variable.
 */
Integer createInteger(Sign sign, IntegerNode *start);

/** @post La representation de l'entier 0 sous une forme a precision variable.
 */
Integer zero(void);

/** Initialise un entier a precision variable d'un seul noeud.
 * @pre Un entier signe dans [-9999; 9999].
 * @post La representation de l'entier sous une forme a precision variable.
 */
Integer singleton(int value);

/** Alloue un nouveau noeud d'un entier a precision variable.
 * @pre La valeur entiere du noeud et un pointeur vers le noeud suivant ;
 * @post Un pointeur vers le nouveau noeud. Une assertion est declenchee si le
 *       noeud n'a pu etre alloue.
 */
IntegerNode *createNode(ValueNode value, IntegerNode *next);

/** @pre Un entier valide dont la memoire allouee doit etre liberee
 */
void freeInteger(Integer integer);

/** Affiche un entier a precision variable sous forme decimale.
 * @pre Un entier a precision variable valide.
 */
void showInteger(Integer integer);

/** Affiche la structure interne d'un entier a precision variable.
 * @pre Un entier a precision variable valide.
 */
void showStructure(Integer integer);

#endif
