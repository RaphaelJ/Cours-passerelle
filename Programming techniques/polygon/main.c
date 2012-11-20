/* Projet de programmation: Calculs sur des entiers en precision arbitraire.
 * Raphael Javaux - Septembre 2012.
 *
 * Ce fichier effectue les tests sur les exemples definis dans l'enonce du
 * cours.
 */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include <assert.h>

#include "integers.h"
#include "comparaisons.h"
#include "arithmetic.h"

/** Teste si fct appliquee a e1 et e2 donne le meme entier que celui represente
 * par la chaine de caracteres res.
 * @pre e1, e2 et res sont des chaines de caracteres codant des entiers.
 * @post Provoque une assertion si le resultat n'est pas correct. Sinon,
 *       affiche le calcul.
 */
void testInteger(
   char *e1, char *e2, Integer (*fct)(Integer e1, Integer e2), char *nomFct,
   char *res
);

int main(void)
{
   /* Verifie que ValeurCalcul et ValeurNoeud peuvent contenir, respectivement
    * les valeurs dans [0; 9999] et [0; 9999^2]. */
   assert (USHRT_MAX >= 9999); // Toujours vrai, le standard impose >= 65535.
   assert ( // Toujours vrai, le standard impose >= (65536^2 - 1).
      ULONG_MAX / 9999 >= 9999
   );
   printf(
      "Les types de la machine hote sont suffisament importants pour contenir "
      "les valeurs necessaires aux calculs.\n"
   );

   // Teste les exemples du cours
   testInteger("4322193", "38200107673", addition, "+", "38204429866");
   testInteger(
      "4322193", "-38200107673", multiplication, "*", "-165108237983486889"
   );
   testInteger("4325", "0", addition, "+", "4325");
   testInteger("4325", "0", multiplication, "*", "0");
   testInteger("38200107673", "4322193", subtraction, "-", "38195785480");
   testInteger("38200107673", "4322193", division, "/", "8838");
   testInteger("38200107673", "4325", addition, "+", "38200111998");
   testInteger("38200107673", "4325", multiplication, "*", "165215465685725");
   testInteger("38200107673", "-4325", multiplication, "*", "-165215465685725");
   testInteger("38200107673", "-4325", subtraction, "-", "38200111998");
   testInteger("38200107673", "4325", division, "/", "8832394");

   return 0;
}

void testInteger(
   char *e1, char *e2, Integer (*fct)(Integer e1, Integer e2), char *fctName,
   char *res
)
{
   Integer e_e1 = readInteger(e1)
         , e_e2 = readInteger(e2)
         , e_res = readInteger(res);

   Integer e_compute = fct(e_e1, e_e2);

   assert(equals(e_res, e_compute));

   freeInteger(e_e1);
   freeInteger(e_e2);
   freeInteger(e_res);
   freeInteger(e_compute);

   printf("%s %s %s = %s\n", e1, fctName, e2, res);
}
