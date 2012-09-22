#include "UnitTestsWrapper.h"

Entier *d_lireEntier(char *chaine)
{
   Entier *res = malloc(sizeof (Entier));
   *res = lireEntier(chaine);
   return res;
}

void d_afficherEntier(Entier *entier)
{
   afficherEntier(*entier);
}

int d_comparer(Entier *e1, Entier *e2)
{
   return comparer(*e1, *e2);
}

Entier *d_addition(Entier *e1, Entier *e2)
{
   Entier *res = malloc(sizeof (Entier));
   *res = addition(*e1, *e2);
   return res;
}

Entier *d_soustraction(Entier *e1, Entier *e2)
{
   Entier *res = malloc(sizeof (Entier));
   *res = soustraction(*e1, *e2);
   return res;
}

Entier *d_multiplication(Entier *e1, Entier *e2)
{
   Entier *res = malloc(sizeof (Entier));
   *res = multiplication(*e1, *e2);
   return res;
}
