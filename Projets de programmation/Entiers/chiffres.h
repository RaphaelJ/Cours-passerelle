#ifndef CHIFFRES_H
#define CHIFFRES_H

#include <stdio.h>
#include <stdlib.h>

#include <assert.h>
#include <stdbool.h>

/** Accepte un caractere en entree et retourne 0 si celui-ci n'est pas un 
 * chiffre, un nombre non-nul si ce n'est pas un chiffre.
 * @post 0 si le caractere n'est pas un chiffre, un nombre non-nul si ce n'est 
 *       pas un chiffre.
 */
bool estChiffre(char caract);

/** Retourne la valeur numerique correspondante a un caractere.
 * @pre Caractere qui doit etre un chiffre ;
 * @post Une valeur numerique dans [0; 9].
 */
unsigned char chiffreVal(char caract);

/** Retourne le caractere representant la valeur d'un chiffre.
 * @pre Une valeur numerique dans [0; 9].
 * @post Le caractere representant le chiffre ;
 */
char chiffreCaract(unsigned char val);

#endif
