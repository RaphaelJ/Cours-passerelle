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
 *       comme etant des nombres positifs. Le signe du resultat est donc
 *       toujours positif.
 */
static Entier __multiplication(Entier e1, Entier e2);

/** @pre Le premier noeud du numerateur, le denominateur et le raport de la
 *       division au cours de la recursion (devra etre donne a 0 pour le
 *       premier noeud du numerateur).
 * @post Un nouvel entier resultat de la division du premier nombre par le
 *       second. La division se fait en considerant les deux nombres
 *       comme etant des nombres positifs. Le signe du resultat est donc
 *       toujours positif.
 */
static Entier __divisionRec(
   const EntierNoeud *numerateur, Entier denominateur, Entier *report
);

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
      Entier res = __addition(e1.debut, e2.debut);
      res.signe = NEGATIF;
      return res;
   }
}

Entier soustraction(Entier e1, Entier e2)
{
   if (e1.signe == POSITIF && e2.signe == POSITIF) // A - B
      return __soustraction(e1, e2);
   else if (e1.signe == POSITIF && e2.signe == NEGATIF)  // A - (-B) = A + B
      return __addition(e1.debut, e2.debut);
   else if (e1.signe == NEGATIF && e2.signe == NEGATIF) // -A - (-B) = B - A
      return __soustraction(e2, e1);
   else // -A - B = -(A + B)
   {
      Entier res = __addition(e1.debut, e2.debut);
      res.signe = NEGATIF;
      return res;
   }
}

Entier multiplication(Entier e1, Entier e2)
{
   Entier res = __multiplication(e1, e2);
   res.signe = e1.signe * e2.signe;
   return res;
}

Entier division(Entier numerateur, Entier denominateur)
{
   assert (!nul(denominateur));

   Entier report = zero();

   Signe signeRes = numerateur.signe * denominateur.signe;
   denominateur.signe = numerateur.signe = POSITIF;
   Entier res = __divisionRec(numerateur.debut, denominateur, &report);
   res.signe = signeRes;

   libererEntier(report);

   return res;
}

static Entier __addition(const EntierNoeud *e1, const EntierNoeud *e2)
{
   Entier res = creerEntier(POSITIF, NULL);

   /* Pointeur vers le pointeur suivant du noeud precedent ou de la tete de
    * liste. */
   EntierNoeud **prec = &(res.debut);

   ValeurCalcul report = 0;
   while (e1 != NULL || e2 != NULL || report != 0)
   {
      ValeurCalcul val1, val2;
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

      ValeurCalcul valRes = val1 + val2 + report;
      report = valRes / 10000;

      EntierNoeud *noeud = creerNoeud(valRes % 10000, NULL);
      *prec = noeud;
      prec = &(noeud->suivant);
   }

   return res;
}

static Entier __soustraction(Entier e1, Entier e2)
{
   e2.signe = e1.signe = POSITIF;

   Signe signeRes;
   /* Soustrait toujours le nombre le plus important au nombre le moins
    * important. */
   if (superieur(e2, e1)) // A - B = - (B - A) si B > A
   {
      signeRes = NEGATIF;

      // Swappe les deux operandes
      Entier tmp = e1;
      e1 = e2;
      e2 = tmp;
   }
   else
      signeRes = POSITIF;

   Entier res = creerEntier(signeRes, NULL);

   /* Maintient une liste temporaire des differences entre noeuds qui nulles
    * pour ne les ajouter au resultat que s'il y a des noeuds non nuls au
    * debut du nombre. */
   Entier resZero = creerEntier(signeRes, NULL);

   EntierNoeud *nodeE1 = e1.debut
             , *nodeE2 = e2.debut;

   EntierNoeud **prec = &(res.debut);
   EntierNoeud **precZero = &(resZero.debut);

   ValeurCalcul report = 0;
   while (nodeE1 != NULL || report != 0)
   {
      ValeurCalcul val1 = nodeE1->valeur
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

      ValeurCalcul valRes = val1 - val2;

      EntierNoeud *noeud = creerNoeud(valRes, NULL);
      *precZero = noeud;

      // Ne rajoute pas directement les zeros au debut du nombre.
      if (valRes != 0)
      {
         /* Si la soustraction n'est pas nulle, on rajoute les noeuds nuls en
          * attente au resultat et on vide cette liste temporaire. */
         *prec = *precZero;
         prec = &(noeud->suivant);

         resZero.debut = NULL;
         precZero = &(resZero.debut);
      }
      else
      {
         // Sinon, on rajoute a la liste des noeuds nuls en attente.
         precZero = &(noeud->suivant);
      }
   }

   libererEntier(resZero); // On ignore les zero au debut du resultat.
   return res;
}

static Entier __multiplication(Entier e1, Entier e2)
{
   if (nul(e1) || nul(e2))
      return zero();

   Entier res = zero();
   EntierNoeud *nodeE1 = e1.debut
             , *nodeE2 = e2.debut;

   /* Multiplie e1 par tous les noeuds de e2 en additionnant successivement
    * les resultats dans res. */
   for (int puissance = 0; nodeE2 != NULL; puissance++)
   {
      Entier tmp = creerEntier(POSITIF, NULL);
      EntierNoeud **prec = &(tmp.debut);

      /* Rajoute un nombre de noeud nuls au resultat temporaire correspondant
       * au numero de la puissance du noeud de e2 en cours de multiplication. */
      for (int i = 0; i < puissance; i++)
      {
         EntierNoeud *noeud = creerNoeud(0, NULL);
         *prec = noeud;
         prec = &(noeud->suivant);
      }

      ValeurCalcul report = 0;
      ValeurCalcul val2 = nodeE2->valeur;
      const EntierNoeud *e1Iter = nodeE1;

      // Multiplie tous les noeuds de e1 par le noeud en cours de e2.
      while (e1Iter != NULL || report != 0)
      {
         ValeurCalcul val1;
         if (e1Iter != NULL)
         {
            val1 = e1Iter->valeur;
            e1Iter = e1Iter->suivant;
         }
         else
            val1 = 0;

         ValeurCalcul valRes = val1 * val2 + report;
         report = valRes / 10000;

         EntierNoeud *noeud = creerNoeud(valRes % 10000, NULL);
         *prec = noeud;
         prec = &(noeud->suivant);
      }

      Entier precRes = res;
      res = addition(precRes, tmp);

      libererEntier(precRes);
      libererEntier(tmp);

      nodeE2 = nodeE2->suivant;
   }

   return res;
}

static Entier __divisionRec(
   const EntierNoeud *numerateur, Entier denominateur, Entier *report
)
{
   if (numerateur != NULL)
   {
      // Effectue d'abord le division sur les noeuds les plus profonds.
      Entier quotient = __divisionRec(
         numerateur->suivant, denominateur, report
      );

      /* Decale le report precedent d'un noeud et y additionne le noeud du
       * numerateur en cours. */
      *report = creerEntier(
         POSITIF, creerNoeud(numerateur->valeur, report->debut)
      );

      // Divise le report par le denominateur si c'est possible.
      if (superieurEgal(*report, denominateur))
      {
         // Le quotient de la division du report par le denominateur ne peut
         // se situer que dans [1; 9999]. Effectue une recherche dicotomique
         // pour trouver cette valeur.
         ValeurCalcul minMult = 1, maxMult = 10000;
         while (maxMult - minMult > 1)
         {
            ValeurCalcul m = (minMult + maxMult) / 2;
            Entier mult = singleton(m);
            Entier produit = multiplication(denominateur, mult);
            libererEntier(mult);

            if (superieur(produit, *report))
               maxMult = m;
            else
               minMult = m;

            libererEntier(produit);
         }

         Entier precReport = *report;
         Entier mult = singleton(minMult);
         Entier produit = multiplication(denominateur, mult);
         *report = soustraction(precReport, produit);

         libererEntier(precReport);
         libererEntier(produit);
         libererEntier(mult);

         return creerEntier(POSITIF, creerNoeud(minMult, quotient.debut));
      }
      else if (!nul(quotient))
         // Sinon, rajoute un 0 si ce n'est pas le premier chiffre du quotient.
         return creerEntier(POSITIF, creerNoeud(0, quotient.debut));
      else
         return zero();
   }
   else
      return zero();
}
