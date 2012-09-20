#include "chiffres.h"

bool estChiffre(char caract)
{
   return caract >= '0' && caract <= '9';
}

int chiffreVal(char caract)
{
   assert (estChiffre(caract));
   return caract - '0';
}

char chiffreCaract(int val)
{
   assert (val >= 0 && val <= 9);
   return (char) val + '0';
}
