#include "chiffres.h"

/** Accepte un caractere en entree et retourne 0 si celui-ci n'est pas un 
 * chiffre, un nombre non-nul si ce n'est pas un chiffre.
 * @post 0 si le caractere n'est pas un chiffre, un nombre non-nul si ce n'est 
 *       pas un chiffre.
 */
int estChiffre(char caract)
{
   return caract >= '0' && caract <= '9';
}

/** Retourne la valeur numerique correspondante a un caractere.
 * @pre Caractere qui doit etre un chiffre ;
 * @post Une valeur numerique dans [0; 9].
 */
int chiffreVal(char caract)
{
   assert (estChiffre(caract));
   return caract - '0';
}


/** Retourne le caractere representant la valeur d'un chiffre.
 * @pre Une valeur numerique dans [0; 9].
 * @post Le caractere representant le chiffre ;
 */
char chiffreCaract(int val)
{
   assert (val >= 0 && val <= 9);
   return (char) val + '0';
}
