#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include <assert.h>

#include "entiers.h"
#include "comparaisons.h"
#include "arithmetique.h"

/** Teste si fct applique a e1 et e2 donne le meme entier que celui represente
 * par la chaine de caracteres res.
 * @pre e1, e2 et res sont des chaines de caracteres respectant codant des 
 *      entiers.
 * @post Provoque une assertion si le resultat n'est pas correct. Sinon, 
 *       affiche le calcul.
 */
void testEntiers(
   char *e1, char *e2, Entier (*fct)(Entier e1, Entier e2), char *nomFct,
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
   testEntiers("4322193", "38200107673", addition, "+", "38204429866");
   testEntiers(
      "4322193", "-38200107673", multiplication, "*", "-165108237983486889"
   );
   testEntiers("4325", "0", addition, "+", "4325");
   testEntiers("4325", "0", multiplication, "*", "0");
   testEntiers("38200107673", "4322193", soustraction, "-", "38195785480");
   testEntiers("38200107673", "4322193", division, "/", "8838");
   testEntiers("38200107673", "4325", addition, "+", "38200111998");
   testEntiers("38200107673", "4325", multiplication, "*", "165215465685725");
   testEntiers("38200107673", "-4325", multiplication, "*", "-165215465685725");
   testEntiers("38200107673", "-4325", soustraction, "-", "38200111998");
   testEntiers("38200107673", "4325", division, "/", "8832394");

   return 0;
}

void testEntiers(
   char *e1, char *e2, Entier (*fct)(Entier e1, Entier e2), char *nomFct,
   char *res
)
{
   Entier e_e1 = lireEntier(e1)
        , e_e2 = lireEntier(e2)
        , e_res = lireEntier(res);

   Entier e_calcul = fct(e_e1, e_e2);

   assert(egal(e_res, e_calcul));

   libererEntier(e_e1);
   libererEntier(e_e2);
   libererEntier(e_res);
   libererEntier(e_calcul);

   printf("%s %s %s = %s\n", e1, nomFct, e2, res);
}
