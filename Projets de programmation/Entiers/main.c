#include <stdio.h>
#include <stdlib.h>

#include "entiers.h"
#include "comparaisons.h"
#include "arithmetique.h"

int main(void)
{
   Entier e1 = creerEntier("4322193")
        , e2 = creerEntier("38200107673")
        , e3 = creerEntier("-38200107673")
        , e4 = creerEntier("4325")
        , e5 = creerEntier("0")
        , e6 = creerEntier("38200107673")
        , e7 = creerEntier("4325")
        , e8 = creerEntier("-4325");

   Entier r1 = addition(e1, e2)
        , r2 = multiplication(e1, e3)
        , r3 = addition(e4, e5)
        , r4 = multiplication(e4, e5)
        , r5 = soustraction(e6, e1)
        , r7 = addition(e6, e7)
        , r8 = multiplication(e6, e7)
        , r9 = multiplication(e6, e8)
        , r10 = soustraction(e6, e8);

   afficherEntier(r1);
   afficherEntier(r2);
   afficherEntier(r3);
   afficherEntier(r4);
   afficherEntier(r5);
   afficherEntier(r7);
   afficherEntier(r8);
   afficherEntier(r9);
   afficherEntier(r10);

   libererEntier(e1);
   libererEntier(e2);
   libererEntier(e3);
   libererEntier(e4);
   libererEntier(e5);
   libererEntier(e6);
   libererEntier(e7);
   libererEntier(e8);

   libererEntier(r1);
   libererEntier(r2);
   libererEntier(r3);
   libererEntier(r4);
   libererEntier(r5);
   libererEntier(r7);
   libererEntier(r8);
   libererEntier(r9);
   libererEntier(r10);

   return 0;
}
