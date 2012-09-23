/* Projet de programmation: Calculs sur des entiers en precision arbitraire.
 * Raphael Javaux - Septembre 2012.
 *
 * Ce fichier contient diverses fonctions utilitaire utilisees lors de la
 * manipulation de caracteres representant des chiffres.
 */

#ifndef DIGITS_H
#define DIGITS_H

#include <stdio.h>
#include <stdlib.h>

#include <assert.h>
#include <stdbool.h>

/** Accepte un caractere en entree et retourne 0 si celui-ci n'est pas un
 * chiffre, un nombre non-nul si ce n'est pas un chiffre.
 * @post 0 si le caractere n'est pas un chiffre, un nombre non-nul si c'est un
 *       chiffre.
 */
bool isDigit(char c);

/** Retourne la valeur numerique correspondante a un caractere.
 * @pre Caractere qui doit etre un chiffre ;
 * @post Une valeur numerique dans [0; 9].
 */
unsigned char digitVal(char c);

/** Retourne le caractere representant la valeur d'un chiffre.
 * @pre Une valeur numerique dans [0; 9] ;
 * @post Le caractere representant le chiffre.
 */
char digitChar(unsigned char val);

#endif
