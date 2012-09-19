#include "comparaisons.h"

int __comparer(Entier e1, Entier e2);

/** @pre Deux nombre entiers valides.
 *  @post 1 si le premier entier est inferieur au second, 0 sinon.
 */
int inferieur(Entier e1, Entier e2)
{
   return (__comparer(e1, e2) < 0) ? 1 : 0;
}

/** @pre Deux nombre entiers valides.
 *  @post 1 si le premier entier est inferieur ou egal au second, 0 sinon.
 */
int inferieurEgal(Entier e1, Entier e2)
{
   return (__comparer(e1, e2) <= 0) ? 1 : 0;
}

/** @pre Deux nombre entiers valides.
 *  @post 1 si le premier entier est plus grand que le second, 0 sinon.
 */
int superieur(Entier e1, Entier e2)
{
   return (__comparer(e1, e2) > 0) ? 1 : 0;
}

/** @pre Deux nombre entiers valides.
 *  @post 1 si le premier entier est plus grand ou egal au second, 0 sinon.
 */
int superieurEgal(Entier e1, Entier e2)
{
   return (__comparer(e1, e2) >= 0) ? 1 : 0;
}

/** @pre Deux nombre entiers valides.
 *  @post 1 si le premier entier est egal au second, 0 sinon.
 */
int egal(Entier e1, Entier e2)
{
   return (__comparer(e1, e2) == 0) ? 1 : 0;
}

/** @pre Deux nombre entiers valides.
 *  @post 1 si le premier entier est different du second, 0 sinon.
 */
int different(Entier e1, Entier e2)
{
   return (__comparer(e1, e2) != 0) ? 1 : 0;
}

int __comparerRec(const EntierNoeud *e1, const EntierNoeud *e2);

/** @pre Deux nombre entiers valides.
 *  @post Un entier negatif si le premier entier est inferieur au second, 0 si 
 *        les deux nombres sont egaux, un entier positif si le second nombre est
 *        plus petit.
 */
int __comparer(Entier e1, Entier e2)
{
   // Compare les signes avant de comparer le contenu.
   if (nul(e1) && nul(e2))
      return 0;
   else if (e1.signe == NEGATIF && e2.signe == POSITIF)
      return -1;
   else if (e1.signe == POSITIF && e2.signe == NEGATIF)
      return 1;
   else // Signes identiques, compare le contenu
   {
      int comp = __comparerRec(e1.debut, e2.debut);

      /* Inverse le signe du resultat de la comparaison si les deux nombres sont
       * negatifs. */
      return (e1.signe == POSITIF) ? comp : -comp;
   }
}

/** @pre Deux noeuds de deux nombres entiers valides.
 *  @post Un entier negatif si le premier entier est inferieur au second, 0 si 
 *        les deux nombres sont egaux, un entier positif si le second nombre est
 *        plus petit.
 */
int __comparerRec(const EntierNoeud *e1, const EntierNoeud *e2)
{
   if (e1 == NULL)
      return (e2 == NULL) ? 0 : -1;
   else if (e2 == NULL)
      return 1;
   else 
   {
      int comp = __comparerRec(e1->suivant, e2->suivant);

      if (comp == 0)
         return e1->valeur - e2->valeur;
      else
         return comp;
   }
}
