#include <string.h>

#include "entiers.h"

#include "chiffres.h"

const Signe POSITIF = +1;
const Signe NEGATIF = -1;

/** Affiche le chiffre a l'ecran. N'affiche pas le chiffre s'il est nul et que
 * premier vaut une valeur non nulle (cas des premiers zeros du nombre).
 * @pre Premier vaut une valeur non nulle dans le cas du premier chiffre du
 *      nombre. Chiffre est dans [0; 9].
 * @post Premier a une valeur nulle si un nombre a ete affiche ou s'il etait 
 *       deja nul.
 */
static void __afficherChiffre(bool *premier, int chiffre);

/** Recurse sur les noeuds de l'entier pour afficher la representation entiere
 * du nombre.
 * @pre Un noeud d'un entier a precision variable valide.
 */
static void __afficherEntierRec(EntierNoeud noeud);

Entier lireEntier(char* chaine)
{
   Signe signe;

   if (chaine[0] == '-') // Nombre negatif
   {
      assert (chaine[1] != '\0'); // Impose au moins un chiffre
      signe = NEGATIF;
      chaine++; // Ignore le signe a partir de maintenant
   }
   else
   {
      assert (chaine[0] != '\0'); // Impose au moins un chiffre
      signe = POSITIF;
   }

   Entier entier = creerEntier(signe, NULL);

   int n = strlen(chaine);

   /* Nombre de chiffres du premier groupe.
    * Ne lit pas 4 chiffres pour le premier groupe du nombre si le nombre 
    * total de chiffres n'est pas un multiple de 4. */
   int nbreChiffres = n % 4;
   if (nbreChiffres == 0)
      nbreChiffres = 4;

   // Itere les groupes de 4 chiffres du nombre, en ajoutant les nouveaux
   // groupes a la tete de la liste.
   bool premier = true;
   for (int i = 0; i < n; )
   {
      int valeur = 0;
      // Itere tous les chiffres composants le groupe
      for (int j = 0; j < nbreChiffres; j++)
      {
         int chiffre = chiffreVal(chaine[i + j]);

         /* Remarque: je pourrais multiplier chiffre par pow(10, x), mais pow de
          * la stdlib fonctionne sur des flotants, et en fait, j'ai eu flemme de
          * reimplementer pow sur des entiers, et puis ca serait plus lent. */
         switch (4 - nbreChiffres + j)
         {
            case 0:
               valeur += chiffre * 1000;
               break;
            case 1:
               valeur += chiffre * 100;
               break;
            case 2:
               valeur += chiffre * 10;
               break;
            case 3:
               valeur += chiffre;
         }
      }

      i += nbreChiffres; // Passe le nombre de chiffres du groupe
      nbreChiffres = 4; // Seul le premier groupe peut ne pas faire 4 chiffres

      if (!premier || valeur > 0) // Ignore les premiers 0 de la chaine
      {
         // Rajoute le noeud a la tete du nombre
         entier.debut = creerNoeud(valeur, entier.debut); 

         premier = false;
      }
   }

   return entier;
}

Entier creerEntier(Signe signe, EntierNoeud *debut)
{
   assert (signe == POSITIF || signe == NEGATIF);
   return (Entier) { .signe = signe, .debut = debut };
}

Entier zero(void)
{
   return creerEntier(POSITIF, NULL);
}

EntierNoeud *creerNoeud(int valeur, EntierNoeud * suivant)
{
   EntierNoeud *noeud = (EntierNoeud *) malloc(sizeof (EntierNoeud));
   assert (noeud);
   noeud->valeur = valeur;
   noeud->suivant = suivant;
   return noeud;
}

void libererEntier(Entier entier)
{
   EntierNoeud *noeud = entier.debut;

   while (noeud != NULL) 
   {
      EntierNoeud *suivant = noeud->suivant;
      free(noeud);
      noeud = suivant;
   }
}

bool nul(Entier entier)
{
   return entier.debut == NULL;
}

int positif(Entier entier)
{
   return !nul(entier) && entier.signe == POSITIF;
}

void afficherEntier(Entier entier)
{
   if (nul(entier))
      printf("0\n");
   else
   {
      if (entier.signe == NEGATIF)
         putchar('-');

      __afficherEntierRec(*(entier.debut));
      putchar('\n');
   }
}

static void __afficherEntierRec(EntierNoeud noeud)
{
   bool premier;

   /* Affiche d'abord les noeuds les plus profonds de la liste a l'aide d'un
    * appel recursif. */
   if (noeud.suivant != NULL) 
   {
      __afficherEntierRec(*(noeud.suivant));
      premier = false;
   }
   else
      /* Va verifier qu'il n'y a pas des 0 au debut du nombre qui ne doivent
       * etre affiches si on est sur le dernier noeud de l'entier. */
      premier = true;

   // Affiche les 4 chiffres
   int nombre = noeud.valeur;
   int chiffre;

   chiffre = nombre / 1000;
   nombre -= chiffre * 1000;
   __afficherChiffre(&premier, chiffre);

   chiffre = nombre / 100;
   nombre -= chiffre * 100;
   __afficherChiffre(&premier, chiffre);

   chiffre = nombre / 10;
   nombre -= chiffre * 10;
   __afficherChiffre(&premier, chiffre);

   chiffre = nombre;
   __afficherChiffre(&premier, chiffre);
}

static void __afficherChiffre(bool *premier, int chiffre)
{
   if (!(*premier) || chiffre != 0)
   {
      putchar(chiffreCaract(chiffre));
      *premier = false;
   }
}
