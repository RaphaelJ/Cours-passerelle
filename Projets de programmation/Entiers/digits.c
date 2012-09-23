/* Projet de programmation: Calculs sur des entiers en precision arbitraire.
 * Raphael Javaux - Septembre 2012.
 *
 * Ce fichier contient diverses fonctions utilitaire utilisees lors de la
 * manipulation de caracteres representant des chiffres.
 */

#include "digits.h"

bool isDigit(char c)
{
   return c >= '0' && c <= '9';
}

unsigned char digitVal(char c)
{
   assert (isDigit(c));
   return (unsigned char) (c - '0');
}

char digitChar(unsigned char val)
{
   assert (/*val >= 0 && */val <= 9);
   return (char) val + '0';
}
