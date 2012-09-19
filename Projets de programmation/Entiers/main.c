#include <stdio.h>
#include <stdlib.h>

#include "entiers.h"
#include "comparaisons.h"

int main(void)
{
   afficherEntier(creerEntier("0"));
//    creerEntier("4325");
   afficherEntier(creerEntier("4325"));
   afficherEntier(creerEntier("123456789"));
   afficherEntier(creerEntier("9876543210"));
   
   printf(
      "%d, %d, %d, %d, %d, %d\n", 
      inferieur(creerEntier("-51515"), creerEntier("0")),
      superieur(creerEntier("9876543210"), creerEntier("9876543212")),
      superieurEgal(creerEntier("9876543212"), creerEntier("9876543210")),
      inferieurEgal(creerEntier("9876543212"), creerEntier("9876543210")),
      egal(creerEntier("9876543212"), creerEntier("9876543210")),
      different(creerEntier("9876543212"), creerEntier("9876543210"))
   );
//    libererEntier();
}
