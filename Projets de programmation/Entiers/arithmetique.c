#include <assert.h>
#include <stdbool.h>

#include "arithmetique.h"

#include "comparaisons.h"

/** @pre Deux noeuds de deux entiers a precision variable valides.
 * @post Un nouvel entier resultat de l'addition des deux nombres. L'addition
 *       se fait en considerant les deux arguments comme etant des nombres
 *       positifs.
 */
static Entier __addition(const EntierNoeud *e1, const EntierNoeud *e2);

/** @pre Deux entiers a precision variable valides.
 * @post Un nouvel entier resultat de la soustraction du premier nombre par le
 *       second. L'addition se fait en considerant les deux arguments comme
 *       etant des nombres positifs.
 */
static Entier __soustraction(Entier e1, Entier e2);

/** @pre Deux noeuds de deux entiers a precision variable valides.
 * @post Un nouvel entier resultat de la soustraction du premier nombre par le
 *       second. La multiplication se fait en considerant les deux arguments 
 *       comme etant des nombres positifs. Le signe du resultat est indefini.
 */
static Entier __multiplication(const EntierNoeud *e1, const EntierNoeud *e2);

Entier addition(Entier e1, Entier e2)
{
   if (e1.signe == POSITIF && e2.signe == POSITIF) // A + B
      return __addition(e1.debut, e2.debut);
   else if (e1.signe == POSITIF && e2.signe == NEGATIF) // A + (-B) = A - B
      return __soustraction(e1, e2);
   else if (e1.signe == NEGATIF && e2.signe == POSITIF) // -A + B = B - A
      return __soustraction(e2, e1);
   else // -A + (-B) = -(A + B)
   {
      e1.signe = POSITIF;
      e2.signe = POSITIF;

      Entier res = __addition(e1.debut, e2.debut);
      res.signe = NEGATIF;
      return res;
   }
}

Entier soustraction(Entier e1, Entier e2)
{
   if (e1.signe == POSITIF && e2.signe == POSITIF) // A - B
      return __soustraction(e1, e2);
   else if (e1.signe == POSITIF && e2.signe == NEGATIF) // A - (-B) = A + B
      return __addition(e1.debut, e2.debut);
   else if (e1.signe == NEGATIF && e2.signe == NEGATIF) // -A - (-B) = B - A
      return __soustraction(e2, e1);
   else // -A - B = -(A + B)
   {
      e1.signe = POSITIF;

      Entier res = __addition(e1.debut, e2.debut);
      res.signe = NEGATIF;
      return res;
   }
}

Entier multiplication(Entier e1, Entier e2)
{
   Entier res = __multiplication(e1.debut, e2.debut);
   res.signe = e1.signe * e2.signe;
   return res;
}

static Entier __addition(const EntierNoeud *e1, const EntierNoeud *e2)
{
   Entier res;
   res.signe = POSITIF;

   /* Pointeur vers le pointeur suivant du noeud precedent ou de la tete de
    * liste. */
   EntierNoeud **prec = &(res.debut);

   int report = 0;
   while (e1 != NULL || e2 != NULL || report != 0)
   {
      EntierNoeud *noeud;

      int val1, val2;
      if (e1 != NULL)
      {
         val1 = e1->valeur;
         e1 = e1->suivant;
      }
      else
         val1 = 0;

      if (e2 != NULL)
      {
         val2 = e2->valeur;
         e2 = e2->suivant;
      }
      else
         val2 = 0;

      int valRes = val1 + val2 + report;
      report = valRes / 10000;

      noeud = creerNoeud(valRes % 10000, NULL);
      *prec = noeud;
      prec = &(noeud->suivant);
   }

   return res;
}

static Entier __soustraction(Entier e1, Entier e2)
{
   Entier res;

   /* Soustrait toujours le nombre le plus important au nombre le moins
    * important. */
   if (superieur(e2, e1)) // A - B = - (B - A) si B > A
   {
      res.signe = NEGATIF;

      // Swappe les deux operandes
      Entier tmp = e1;
      e1 = e2;
      e2 = tmp;
   }
   else
      res.signe = POSITIF;

   EntierNoeud *nodeE1 = e1.debut
             , *nodeE2 = e2.debut;

   EntierNoeud **prec = &(res.debut);

   int report = 0;
   while (nodeE1 != NULL || report != 0)
   {
      EntierNoeud *noeud;
      int val1 = nodeE1->valeur
        , val2;

      nodeE1 = nodeE1->suivant;

      if (nodeE2 != NULL) 
      {
         val2 = nodeE2->valeur;
         nodeE2 = nodeE2->suivant;
      }
      else
         val2 = 0;

      val1 -= report;

      // Effectue un report
      if (val1 < val2)
      {
         val1 += 10000;
         report = 1;
      }
      else
         report = 0;

      noeud = creerNoeud(val1 - val2, NULL);
      *prec = noeud;
      prec = &(noeud->suivant);
   }

   return res;
}

static Entier __multiplication(const EntierNoeud *e1, const EntierNoeud *e2)
{
   Entier res = zero();

   /* Multiplie e1 par tous les noeuds de e2 en additionnant successivement
    * les resultats dans res. */
   for (int puissance = 0; e2 != NULL; puissance++)
   {
      Entier tmp;
      tmp.signe = POSITIF;
      EntierNoeud **prec = &(tmp.debut);

      /* Rajoute un nombre de noeud nuls au resultat temporaire correspondant
       * au numero de la puissance du noeud de e2 en cours de multiplication. */
      for (int i = 0; i < puissance; i++)
      {
         EntierNoeud *noeud = creerNoeud(0, NULL);
         *prec = noeud;
         prec = &(noeud->suivant);
      }

      int report = 0;
      int val2 = e2->valeur;

      // Multiplie tous les noeuds de e1 par le noeud en cours de e2.
      while (e1 != NULL || report != 0)
      {
         int val1;
         if (e1 != NULL)
         {
            val1 = e1->valeur;
            e1 = e1->suivant;
         }
         else
            val1 = 0;

         int valRes = val1 * val2 + report;
         report = valRes / 10000;

         EntierNoeud *noeud = creerNoeud(valRes % 10000, NULL);
         *prec = noeud;
         prec = &(noeud->suivant);
      }

      Entier precRes = res;
      res = addition(precRes, tmp);

      libererEntier(precRes);
      libererEntier(tmp);

      e2 = e2->suivant;
   }

   return res;
}
