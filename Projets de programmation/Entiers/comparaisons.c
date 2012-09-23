/* Projet de programmation: Calculs sur des entiers en precision arbitraire.
 * Raphael Javaux - Septembre 2012.
 *
 * Ce fichier contient toutes les operations de comparaison (compare, <, <=, >,
 * >=, == et !=) utilisables sur les entiers a precision variables.
 */

#include "comparaisons.h"

/** @pre Deux noeuds de deux nombres entiers valides ;
 *  @post Un entier negatif si le premier entier est inferieur au second, 0 si
 *        les deux nombres sont egaux, un entier positif si le second nombre est
 *        plus petit.
 */
static int __compareRec(const IntegerNode *e1, const IntegerNode *e2);

int compare(Integer e1, Integer e2)
{
   // Compare les signes avant de comparer le contenu.
   if (isZero(e1) && isZero(e2))
      return 0;
   else if (e1.sign == NEGATIVE && e2.sign == POSITIVE)
      return -1;
   else if (e1.sign == POSITIVE && e2.sign == NEGATIVE)
      return 1;
   else // Signes identiques, compare le contenu
   {
      int comp = __compareRec(e1.start, e2.start);

      /* Inverse le signe du resultat de la comparaison si les deux nombres sont
       * negatifs. */
      return (e1.sign == POSITIVE) ? comp : -comp;
   }
}

int inferior(Integer e1, Integer e2)
{
   return (compare(e1, e2) < 0) ? 1 : 0;
}

int inferiorEquals(Integer e1, Integer e2)
{
   return (compare(e1, e2) <= 0) ? 1 : 0;
}

int superior(Integer e1, Integer e2)
{
   return (compare(e1, e2) > 0) ? 1 : 0;
}

int superiorEquals(Integer e1, Integer e2)
{
   return (compare(e1, e2) >= 0) ? 1 : 0;
}

int equals(Integer e1, Integer e2)
{
   return (compare(e1, e2) == 0) ? 1 : 0;
}

int different(Integer e1, Integer e2)
{
   return (compare(e1, e2) != 0) ? 1 : 0;
}

bool isZero(Integer integer)
{
   return integer.start == NULL;
}

bool isPositive(Integer integer)
{
   return !isZero(integer) && integer.sign == POSITIVE;
}

static int __compareRec(const IntegerNode *e1, const IntegerNode *e2)
{
   if (e1 == NULL)
      return (e2 == NULL) ? 0 : -1;
   else if (e2 == NULL)
      return 1;
   else 
   {
      int comp = __compareRec(e1->next, e2->next);

      if (comp == 0)
         return e1->value - e2->value;
      else
         return comp;
   }
}
