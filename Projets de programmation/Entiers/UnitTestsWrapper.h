#ifndef UNITTESTSWRAPPER_H
#define UNITTESTSWRAPPER_H

#include <stdio.h>
#include <stdlib.h>

#include "entiers.h"
#include "comparaisons.h"
#include "arithmetique.h"

Entier *d_lireEntier(char *chaine);
void d_afficherEntier(Entier *entier);

int d_comparer(Entier *e1, Entier *e2);

Entier *d_addition(Entier *e1, Entier *e2);
Entier *d_soustraction(Entier *e1, Entier *e2);
Entier *d_multiplication(Entier *e1, Entier *e2);
Entier *d_division(Entier *numerateur, Entier *denominateur);

#endif
