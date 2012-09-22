#include "chiffres.h"

bool estChiffre(char caract)
{
   return caract >= '0' && caract <= '9';
}

unsigned char chiffreVal(char caract)
{
   assert (estChiffre(caract));
   return (unsigned char) (caract - '0');
}

char chiffreCaract(unsigned char val)
{
   assert (/*val >= 0 && */val <= 9);
   return (char) val + '0';
}
