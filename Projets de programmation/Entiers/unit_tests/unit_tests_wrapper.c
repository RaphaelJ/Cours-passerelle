/* Projet de programmation: Calculs sur des entiers en precision arbitraire.
 * Raphael Javaux - Septembre 2012.
 *
 * Ce fichier contient des wrappers vers les fonctions du projets qui sont
 * uniquement utilises par le programme de test unitaire ecrit en Haskell qui
 * impose que toutes les donnees passees au programme C le soient sous forme
 * de pointeurs.
 * Les fonctions ci-dessous sont donc de simple wrappers vers les fonctions
 * du meme nom du projet mais qui retournent et prennent des pointeurs vers des 
 * entiers.
 */

#include "unit_tests_wrapper.h"

Integer *d_readInteger(char *str)
{
   Integer *res = malloc(sizeof (Integer));
   *res = readInteger(str);
   return res;
}

void d_showInteger(Integer *integer)
{
   showInteger(*integer);
}

int d_compare(Integer *i1, Integer *i2)
{
   return compare(*i1, *i2);
}

Integer *d_addition(Integer *i1, Integer *i2)
{
   Integer *res = malloc(sizeof (Integer));
   *res = addition(*i1, *i2);
   return res;
}

Integer *d_subtraction(Integer *i1, Integer *i2)
{
   Integer *res = malloc(sizeof (Integer));
   *res = subtraction(*i1, *i2);
   return res;
}

Integer *d_multiplication(Integer *i1, Integer *i2)
{
   Integer *res = malloc(sizeof (Integer));
   *res = multiplication(*i1, *i2);
   return res;
}

Integer *d_division(Integer *numerator, Integer *denominator)
{
   Integer *res = malloc(sizeof (Integer));
   *res = division(*numerator, *denominator);
   return res;
}
