#include "arithmetique.h"

/** Accepte une chaine de caracteres en entree et retourne l'entier en precision
 * variable correspondant.
 * @pre Une chaine doit etre composee d'un ou plusieurs caracteres numeriques et
 *      peut debuter avec le caractere "-".
 * @post Retourne la representation de l'entier sous une forme a precision 
 *       variable.
 *       Les deux nombres donnes en entree ne sont pas modifies.
 */
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

Entier __addition(const EntierNoeud *e1, const EntierNoeud *e2)
{
   Entier res;
   res.signe = POSITIF;

   EntierNoeud **prec = &(res.debut);

   int report = 0;
   while (e1 != NULL || e2 != NULL || report != 0)
   {
      EntierNoeud *noeud;
      int val1 = (e1 != NULL) ? e1.valeur : 0;
        , val2 = (e2 != NULL) ? e2.valeur : 0;

      int valRes = val1 + val2 + report;
      report = valRes / 10000;

      noeud = creerNoeud(valRes % 10000, NULL);
      *prec = noeud;
      prec = &(noeud.suivant);

      e1 = e1.suivant;
      e2 = e2.suivant;
   }

   return res;
}

Entier __soustraction(Entier e1, Entier e2)
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
      int val1 = nodeE1.valeur;
        , val2 = (nodeE2 != NULL) ? nodeE2.valeur : 0;

      val1 -= report;

      // Effecture un report
      if (val1 < val2)
      {
         val1 += 10000;
         report = 1;
      }

      int valRes = val1 - val2;

      noeud = creerNoeud(valRes, NULL);
      *prec = noeud;
      prec = &(noeud.suivant);

      nodeE1 = nodeE1.suivant;
      nodeE1 = nodeE1.suivant;
   }

   return res;
}
