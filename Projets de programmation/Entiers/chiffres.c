#include "chiffres.h"

/** Accepte un caractere en entree et retourne 0 si celui-ci n'est pas un 
 * chiffre, un nombre non-nul si ce n'est pas un chiffre.
 * @post 0 si le caractere n'est pas un chiffre, un nombre non-nul si ce n'est 
 *       pas un chiffre.
 */
int estChiffre(const char caract)
{
   return caract >= '0' && caract <= '9';
}

/** Retourne la valeur numerique correspondante a un caractere.
 * @pre Caractere doit un chiffre ;
 * @post Une valeur numerique dans [0; 9].
 */
int chiffreVal(const char caract)
{
   assert (estChiffre(caract));
   
   return caract - '0';
}