#include "entiers.h"

/** Accepte une chaine de caracteres en entree et retourne l'entier en precision
 * variable correspondant.
 * @pre chaine doit etre composee d'un ou plusieurs caracteres numeriques et 
 *      peut debuter avec le caractere "-".
 */
Entier creerEntier(char* chaine)
{
   Entier entier;

   if (chaine[0] == '-') // Nombre negatif
   {
      assert (chaine[1] != '\0');
      entier.signe = NEGATIF;
      chaine++; // Ignore le signe a partir de maintenant
   }
   else 
   {
      assert (chaine[0] != '\0');
      entier.signe = POSITIF;
   }

   int n = strlen(chaine);

   for (int i = 0; i < n; i += 3)
   {
      noeud = (EntierNoeud *) malloc(EntierNoeud);
      assert(noeud);
      noeud->valeur = 0;

      // Itere tous les chiffres composants le noeud
      int nbreChiffres = min(3, n - i);
      for (int j = 0; j < nbreChiffres; j++)
      {
         int val = chiffreVal(chaine[i + j]);

         switch (j)
         {
            case 0:
               noeud->valeur = val;
               break;
            case 1:
               noeud->valeur = val * 10;
               break;
            case 2:
               noeud->valeur = val * 100;
               break;
         }
      }

      entier.debut = noeud;
   }
}
