#include "entiers.h"

/** Accepte une chaine de caracteres en entree et retourne l'entier en precision
 * variable correspondant.
 * @pre chaine doit etre composee d'un ou plusieurs caracteres numeriques et
 *      peut debuter avec le caractere "-".
 */
Entier creerEntier(char* chaine)
{
   Entier entier;
   entier.debut = NULL;

   if (chaine[0] == '-') // Nombre negatif
   {
      assert (chaine[1] != '\0'); // Impose au moins un chiffre
      entier.signe = NEGATIF;
      chaine++; // Ignore le signe a partir de maintenant
   }
   else
   {
      assert (chaine[0] != '\0'); // Impose au moins un chiffre
      entier.signe = POSITIF;
   }

   if (n == 1 && chaine[0] == '0') // Codage de 0
      return entier;

   // Itere les groupes de 4 chiffres du nombre, en ajoutant les nouveaux
   // groupes a la tete de la liste.
   int nbreChiffres = 0;
   for (int i = 0; i < n; )
   {
      EntierNoeud *noeud = (EntierNoeud *) malloc(sizeof (EntierNoeud));
      assert(noeud);
      noeud->valeur = 0;
      noeud->suivant = entier.debut;

 
      /* Nombre de chiffres du groupe.
       * Ne lit pas 4 chiffres pour le premier groupe du nombre si le nombre 
       * total de chiffres n'est pas un multiple de 4. */
      int nbreChiffres = 4 - (n - i) % 4; 

      // Itere tous les chiffres composants le groupe
      for (int j = 0; j < nbreChiffres; j++)
      {
         int val = chiffreVal(chaine[i + j]);

         /* Remarque: je pourrais multiplier val par pow(10, j), mais pow de la
          * stdlib fonctionne sur des flotants, et en fait, j'ai eu flemme de
          * reimplementer pow sur des entiers, et puis ca serait plus lent. */
         switch (j)
         {
            case 0:
               noeud->valeur = val * 1000;
               break;
            case 1:
               noeud->valeur = val * 100;
               break;
            case 2:
               noeud->valeur = val * 10;
               break;
            case 3:
               noeud->valeur = val;
         }
      }

      i += nbreChiffres; // Passe le nombre de chiffres du groupe

      entier.debut = noeud; // Rajoute le noeud a la tete du nombre
   }

   return entier;
}

/** Affiche le chiffre a l'ecran. N'affiche pas le chiffre s'il est nul et que
 * premier vaut une valeur non nulle (cas des premiers zeros du nombre).
 * @pre Premier vaut une valeur non nulle dans le cas du premier chiffre du
 *      nombre. Chiffre est dans [0; 9].
 * @post Premier a une valeur nulle si un nombre a ete affiche ou s'il etait 
 *       deja nul.
 */
void afficherChiffre(int *premier, int chiffre)
{
   if (!(*premier) || chiffre != 0)
   {
      putchar(chiffreCaract(chiffre));
      *premier = 0;
   }
}

void afficherEntierRec(EntierNoeud noeud)
{
   int premier;

   /* Affiche d'abord les noeuds les plus profonds de la liste a l'aide d'un
    * appel recursif. */
   if (noeud.suivant != NULL) 
   {
      afficherEntierRec(*(noeud.suivant));
      premier = 0;
   }
   else
   {
      /* Va verifier qu'il n'y a pas des 0 au debut du nombre qui ne doivent
       * etre affiches. */
      premier = 1; 
   }

   // Affiche les 4 chiffres
   int nombre = noeud.valeur;
   int chiffre;

   chiffre = noeud.valeur / 1000;
   nombre -= chiffre * 1000;
   afficherChiffre(&premier, chiffre);

   chiffre = noeud.valeur / 100;
   nombre -= chiffre * 100;
   afficherChiffre(&premier, chiffre);

   chiffre = noeud.valeur / 10;
   nombre -= chiffre * 10;
   afficherChiffre(&premier, chiffre);

   chiffre = noeud.valeur;
   afficherChiffre(&premier, chiffre);
}

void afficherEntier(Entier entier)
{
   if (entier.debut == NULL)
      printf("0\n");
   else
   {
      if (entier.signe == NEGATIF)
         putchar('-');

      afficherEntierRec(*(entier.debut));
      putchar('\n');
   }
}
