/* Projet de programmation: Calculs sur des entiers en precision arbitraire.
 * Raphael Javaux - Septembre 2012.
 *
 * Ce fichier contient les fonctions permettant d'initialiser, d'afficher et de
 * liberer les entiers en precision variable.
 * Ce fichier contient egalement les structures de donnees utilisees pour
 * definir ces entiers.
 */

#include <string.h>

#include <assert.h>
#include <stdbool.h>

#include "integers.h"

#include "comparaisons.h"
#include "digits.h"

const Sign POSITIVE = +1;
const Sign NEGATIVE = -1;

/** Affiche le chiffre a l'ecran. N'affiche pas le chiffre s'il est nul et que
 * first vaut une value non nulle (cas des premiers zeros du nombre).
 * @pre Chiffre est dans [0; 9].
 * @post Premier a une value nulle si un nombre a ete affiche ou s'il etait
 *       deja nul.
 */
static void __showDigit(bool *first, int digit);

/** Recurse sur les noeuds de l'entier pour afficher la representation entiere
 * du nombre.
 * @pre Un noeuds d'un entier a precision variable valide.
 */
static void __showIntegerRec(IntegerNode node);

Integer readInteger(char* str)
{
   Sign sign;

   if (str[0] == '-') // Nombre negatif
   {
      assert (str[1] != '\0'); // Impose au moins un chiffre
      sign = NEGATIVE;
      str++; // Passe le signe
   }
   else
   {
      assert (str[0] != '\0'); // Impose au moins un chiffre
      sign = POSITIVE;
   }

   Integer integer = createInteger(sign, NULL);

   int n = strlen(str);

   /* Nombre de chiffres du premier groupe.
    * Ne lit pas 4 chiffres pour le premier groupe du nombre si le nombre
    * total de chiffres n'est pas un multiple de 4. */
   int nDigits = n % 4;
   if (nDigits == 0)
      nDigits = 4;

   // Itere les groupes de 4 chiffres du nombre, en ajoutant les nouveaux
   // groupes a la tete de la liste.
   bool first = true;
   for (int i = 0; i < n; )
   {
      ValueNode value = 0;
      // Itere tous les chiffres composants le groupe
      for (int j = 0; j < nDigits; j++)
      {
         unsigned char digit = digitVal(str[i + j]);

         /* Remarque: je pourrais multiplier chiffre par pow(10, x), mais pow de
          * la stdlib fonctionne sur des flotants, et en fait, j'ai eu flemme de
          * reimplementer pow sur des entiers, et puis ca serait plus lent. */
         switch (4 - nDigits + j)
         {
            case 0:
               value += digit * 1000;
               break;
            case 1:
               value += digit * 100;
               break;
            case 2:
               value += digit * 10;
               break;
            case 3:
               value += digit;
         }
      }

      i += nDigits; // Passe le nombre de chiffres du groupe
      nDigits = 4; // Seul le premier groupe peut ne pas faire 4 chiffres

      if (!first || value > 0) // Ignore les premiers 0 de la chaine.
      {
         // Rajoute le noeud a la tete du nombre
         integer.start = createNode(value, integer.start);

         first = false;
      }
   }

   return integer;
}

Integer createInteger(Sign sign, IntegerNode *start)
{
   assert (sign == POSITIVE || sign == NEGATIVE);
   return (Integer) { .sign = sign, .start = start };
}

Integer zero(void)
{
   return createInteger(POSITIVE, NULL);
}

Integer singleton(int value)
{
   if (value == 0)
      return zero();
   else
   {
      assert (value >= -9999 && value <= 9999);
      Sign sign = value < 0 ? NEGATIVE : POSITIVE;
      return createInteger(
         sign, createNode((ValueNode) (value * sign), NULL)
      );
   }
}

IntegerNode *createNode(ValueNode value, IntegerNode *next)
{
   IntegerNode *node = (IntegerNode *) malloc(sizeof (IntegerNode));
   assert (node);
   node->value = value;
   node->next = next;
   return node;
}

void freeInteger(Integer integer)
{
   IntegerNode *node = integer.start;

   while (node != NULL) 
   {
      IntegerNode *next = node->next;
      free(node);
      node = next;
   }
}

void showInteger(Integer integer)
{
   if (isZero(integer))
      printf("0\n");
   else
   {
      if (integer.sign == NEGATIVE)
         putchar('-');

      __showIntegerRec(*(integer.start));
      putchar('\n');
   }
}

void showStructure(Integer integer)
{
   if (integer.sign == POSITIVE)
      printf("+ -> ");
   else
      printf("- -> ");

   IntegerNode *node = integer.start;
   while (node != NULL)
   {
      printf("%d -> ", node->value);
      node = node->next;
   }

   printf("NULL\n");
}

static void __showIntegerRec(IntegerNode node)
{
   bool first;

   /* Affiche d'abord les noeuds les plus profonds de la liste a l'aide d'un
    * appel recursif. */
   if (node.next != NULL) 
   {
      __showIntegerRec(*(node.next));
      first = false;
   }
   else
      /* Va verifier qu'il n'y a pas des 0 au debut du nombre qui ne doivent
       * etre affiches si on est sur le dernier noeuds de l'entier. */
      first = true;

   // Affiche les 4 chiffres
   ValueNode number = node.value;
   ValueNode digit;

   digit = number / 1000;
   number -= digit * 1000;
   __showDigit(&first, digit);

   digit = number / 100;
   number -= digit * 100;
   __showDigit(&first, digit);

   digit = number / 10;
   number -= digit * 10;
   __showDigit(&first, digit);

   digit = number;
   __showDigit(&first, digit);
}

static void __showDigit(bool *first, int digit)
{
   if (!(*first) || digit != 0)
   {
      putchar(digitChar(digit));
      *first = false;
   }
}
