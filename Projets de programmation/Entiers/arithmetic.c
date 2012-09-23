/* Projet de programmation: Calculs sur des entiers en precision arbitraire.
 * Raphael Javaux - Septembre 2012.
 *
 * Ce fichier contient toutes les operations arithmetiques (+, -, * et /)
 * utilisables sur les entiers a precision variables.
 */

#include <assert.h>
#include <stdbool.h>

#include "arithmetic.h"

#include "comparaisons.h"

/** @pre Deux noeuds de deux entiers a precision variable valides ;
 * @post Un nouvel entier resultat de l'addition des deux nombres. L'addition
 *       se fait en considerant les deux arguments comme etant des nombres
 *       positifs.
 */
static Integer __addition(const IntegerNode *i1, const IntegerNode *i2);

/** @pre Deux entiers a precision variable valides ;
 * @post Un nouvel entier resultat de la soustraction du premier nombre par le
 *       second. L'addition se fait en considerant les deux arguments comme
 *       etant des nombres positifs.
 */
static Integer __subtraction(Integer i1, Integer i2);

/** @pre Deux noeuds de deux entiers a precision variable valides ;
 * @post Un nouvel entier resultat de la soustraction du premier nombre par le
 *       second. La multiplication se fait en considerant les deux arguments
 *       comme etant des nombres positifs. Le signe du resultat est donc
 *       toujours positif.
 */
static Integer __multiplication(Integer i1, Integer i2);

/** @pre Le premier noeud du numerateur, le denominateur et le raport de la
 *       division au cours de la recursion (devra etre donne a 0 pour le
 *       premier noeud du numerateur) ;
 * @post Un nouvel entier resultat de la division entiere du premier nombre par
 *       le second. La division se fait en considerant les deux nombres comme
 *       etant des nombres positifs. Le signe du resultat est donc toujours
 *       positif.
 */
static Integer __divisionRec(
   const IntegerNode *numerator, Integer denominator, Integer *remainder
);

Integer addition(Integer i1, Integer i2)
{
   if (i1.sign == POSITIVE && i2.sign == POSITIVE) // A + B
      return __addition(i1.start, i2.start);
   else if (i1.sign == POSITIVE && i2.sign == NEGATIVE) // A + (-B) = A - B
      return __subtraction(i1, i2);
   else if (i1.sign == NEGATIVE && i2.sign == POSITIVE) // -A + B = B - A
      return __subtraction(i2, i1);
   else // -A + (-B) = -(A + B)
   {
      Integer res = __addition(i1.start, i2.start);
      res.sign = NEGATIVE;
      return res;
   }
}

Integer subtraction(Integer i1, Integer i2)
{
   if (i1.sign == POSITIVE && i2.sign == POSITIVE) // A - B
      return __subtraction(i1, i2);
   else if (i1.sign == POSITIVE && i2.sign == NEGATIVE)  // A - (-B) = A + B
      return __addition(i1.start, i2.start);
   else if (i1.sign == NEGATIVE && i2.sign == NEGATIVE) // -A - (-B) = B - A
      return __subtraction(i2, i1);
   else // -A - B = -(A + B)
   {
      Integer res = __addition(i1.start, i2.start);
      res.sign = NEGATIVE;
      return res;
   }
}

Integer multiplication(Integer i1, Integer i2)
{
   Integer res = __multiplication(i1, i2);
   res.sign = i1.sign * i2.sign;
   return res;
}

Integer division(Integer numerator, Integer denominator)
{
   assert (!isZero(denominator));

   Integer remainder = zero();

   Sign signRes = numerator.sign * denominator.sign;
   denominator.sign = numerator.sign = POSITIVE;
   Integer res = __divisionRec(numerator.start, denominator, &remainder);
   res.sign = signRes;

   freeInteger(remainder);

   return res;
}

static Integer __addition(const IntegerNode *i1, const IntegerNode *i2)
{
   Integer res = createInteger(POSITIVE, NULL);

   /* Pointeur vers le pointeur suivant du noeud precedent ou de la tete de
    * liste. */
   IntegerNode **prec = &(res.start);

   ValueArithm remainder = 0;
   while (i1 != NULL || i2 != NULL || remainder != 0)
   {
      ValueArithm val1, val2;
      if (i1 != NULL)
      {
         val1 = i1->value;
         i1 = i1->next;
      }
      else
         val1 = 0;

      if (i2 != NULL)
      {
         val2 = i2->value;
         i2 = i2->next;
      }
      else
         val2 = 0;

      ValueArithm valRes = val1 + val2 + remainder;
      remainder = valRes / 10000;

      IntegerNode *noeud = createNode(valRes % 10000, NULL);
      *prec = noeud;
      prec = &(noeud->next);
   }

   return res;
}

static Integer __subtraction(Integer i1, Integer i2)
{
   i2.sign = i1.sign = POSITIVE;

   Sign signRes;
   /* Soustrait toujours le nombre le plus important au nombre le moins
    * important. */
   if (superior(i2, i1)) // A - B = - (B - A) si B > A
   {
      signRes = NEGATIVE;

      // Swappe les deux operandes
      Integer tmp = i1;
      i1 = i2;
      i2 = tmp;
   }
   else
      signRes = POSITIVE;

   Integer res = createInteger(signRes, NULL);

   /* Maintient une liste temporaire des differences entre noeuds qui nulles
    * pour ne les ajouter au resultat que s'il y a des noeuds non nuls au
    * debut du nombre. */
   Integer resZero = createInteger(signRes, NULL);

   IntegerNode *nodeI1 = i1.start
             , *nodeI2 = i2.start;

   IntegerNode **prec = &(res.start);
   IntegerNode **precZero = &(resZero.start);

   ValueArithm remainder = 0;
   while (nodeI1 != NULL || remainder != 0)
   {
      ValueArithm val1 = nodeI1->value
                 , val2;

      nodeI1 = nodeI1->next;

      if (nodeI2 != NULL) 
      {
         val2 = nodeI2->value;
         nodeI2 = nodeI2->next;
      }
      else
         val2 = 0;

      val1 -= remainder;

      // Effectue un report
      if (val1 < val2)
      {
         val1 += 10000;
         remainder = 1;
      }
      else
         remainder = 0;

      ValueArithm valRes = val1 - val2;

      IntegerNode *noeud = createNode(valRes, NULL);
      *precZero = noeud;

      // Ne rajoute pas directement les zeros au debut du nombre.
      if (valRes != 0)
      {
         /* Si la soustraction n'est pas nulle, on rajoute les noeuds nuls en
          * attente au resultat et on vide cette liste temporaire. */
         *prec = *precZero;
         prec = &(noeud->next);

         resZero.start = NULL;
         precZero = &(resZero.start);
      }
      else
      {
         // Sinon, on rajoute a la liste des noeuds nuls en attente.
         precZero = &(noeud->next);
      }
   }

   freeInteger(resZero); // On ignore les zero au debut du resultat.
   return res;
}

static Integer __multiplication(Integer i1, Integer i2)
{
   if (isZero(i1) || isZero(i2))
      return zero();

   Integer res = zero();
   IntegerNode *nodeI1 = i1.start
             , *nodeI2 = i2.start;

   /* Multiplie i1 par tous les noeuds de i2 en additionnant successivement
    * les resultats dans res. */
   for (int padding = 0; nodeI2 != NULL; padding++)
   {
      Integer tmp = createInteger(POSITIVE, NULL);
      IntegerNode **prec = &(tmp.start);

      /* Rajoute un nombre de noeud nuls au resultat temporaire correspondant
       * au numero de la puissance du noeud de i2 en cours de multiplication. */
      for (int i = 0; i < padding; i++)
      {
         IntegerNode *noeud = createNode(0, NULL);
         *prec = noeud;
         prec = &(noeud->next);
      }

      ValueArithm remainder = 0;
      ValueArithm val2 = nodeI2->value;
      const IntegerNode *i1Iter = nodeI1;

      // Multiplie tous les noeuds de i1 par le noeud en cours de i2.
      while (i1Iter != NULL || remainder != 0)
      {
         ValueArithm val1;
         if (i1Iter != NULL)
         {
            val1 = i1Iter->value;
            i1Iter = i1Iter->next;
         }
         else
            val1 = 0;

         ValueArithm valRes = val1 * val2 + remainder;
         remainder = valRes / 10000;

         IntegerNode *noeud = createNode(valRes % 10000, NULL);
         *prec = noeud;
         prec = &(noeud->next);
      }

      Integer precRes = res;
      res = addition(precRes, tmp);

      freeInteger(precRes);
      freeInteger(tmp);

      nodeI2 = nodeI2->next;
   }

   return res;
}

static Integer __divisionRec(
   const IntegerNode *numerator, Integer denominator, Integer *remainder
)
{
   if (numerator != NULL)
   {
      // Effectue d'abord le division sur les noeuds les plus profonds.
      Integer quotient = __divisionRec(
         numerator->next, denominator, remainder
      );

      /* Decale le report precedent d'un noeud et y additionne le noeud du
       * numerateur en cours. */
      *remainder = createInteger(
         POSITIVE, createNode(numerator->value, remainder->start)
      );

      // Divise le report par le denominateur si c'est possible.
      if (superiorEquals(*remainder, denominator))
      {
         // Le quotient de la division du report par le denominateur ne peut
         // se situer que dans [1; 9999]. Effectue une recherche dicotomique
         // pour trouver cette valeur.
         ValueArithm minMult = 1, maxMult = 10000;
         while (maxMult - minMult > 1) // Reduit l'intervale [minMult; maxMult[
         {
            ValueArithm m = (minMult + maxMult) / 2;
            Integer mult = singleton(m);
            Integer product = multiplication(denominator, mult);
            freeInteger(mult);

            if (superior(product, *remainder))
               maxMult = m;
            else
               minMult = m;

            freeInteger(product);
         }

         Integer precReport = *remainder;
         Integer mult = singleton(minMult);
         Integer product = multiplication(denominator, mult);
         *remainder = subtraction(precReport, product);

         freeInteger(precReport);
         freeInteger(product);
         freeInteger(mult);

         return createInteger(POSITIVE, createNode(minMult, quotient.start));
      }
      else if (!isZero(quotient))
         // Sinon, rajoute un 0 si ce n'est pas le premier chiffre du quotient.
         return createInteger(POSITIVE, createNode(0, quotient.start));
      else
         return zero();
   }
   else
      return zero();
}
