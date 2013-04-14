{-# LANGUAGE GADTs #-}
-- | Définit les types liés à la définition de l\'arbre syntaxique typé.
--
-- Cet arbre syntaxique est produit par le vérificateur sémantique. Il est plus
-- strict que l\''AST' produit par le parseur.
--
-- L\'utilisation des types algébriques /généralisés/ permet de garantir que
-- chaque instance de l\'arbre syntaxique sera correctement typée (en d\'autres
-- mots, que le vérificateur de types ne peut générer un arbre syntaxique qui
-- ne respecterait pas les règles de typage définies ci-dessus). Cette assertion
-- est garantie par le vérificateur de types de Haskell, qui est un vérificateur
-- de preuves étendu.
--
-- Par exemple l\'expression suivante :
--
-- > int a = true;
--
-- Peut être représentée dans la version non généralisée/typée de l\''AST' mais
-- ne saurait être représentée dans cette version typée.
--
-- L'arbre syntaxique suivant utilise le système de typage de Haskell pour
-- valider (entre-autres) les assertions suivantes :
--
-- * Le type de retour déclaré d\'une fonction est le même que celui retourné
-- par le bloc de code qui de son implémentation ;
--
-- * Les variables déclarées hors des arguments d\'une fonction ont l\'ensemble
-- des dimensions de leurs tableaux renseignées ;
--
-- * Une variable déclarée sans être initialisée ne peut être déclarée
-- constante ;
--
-- * La type de retour de la dernière instruction d\'un bloc détermine le type
-- de retour de ce bloc, un bloc vide n\'a pas de valeur de retour ;
--
-- * Une variable déclarée constante ne peut être assignée en dehors de sa
-- déclaration ;
--
-- * Les deux blocs composant un if doivent retourner le même type de donnée ;
--
-- * Les fonctions et les tableaux doivent être complètement appliqués (càd que
-- tous leurs arguments/indices doivent être renseignés) pour être utilisé dans
-- une expression ;
--
-- * Les types des expressions passées en argument à un appel de fonction 
-- doivent correspondre aux types déclarés de la fonction ;
--
-- * Les types des termes et de retour des opérateurs binaires sont respectés
-- (par exemple Int -> Int -> Bool pour les opérateurs de comparaison ou
-- Int -> Int -> Int pour les opérateurs arithmétiques).
--
-- De plus, le compilateur peut également garantir qu\'il existe un chemin
-- d\'exécution dans le générateur de code pour toute instance de cet AST,
-- quelque soit la combinaison des types de ce dernier, ce qui ne serait pas
-- possible dans un langage orienté objet, par exemple.
--
-- Contrairement à l\''AST' non typé, les références aux variables et aux
-- fonctions ne se font plus via des identifieurs mais via des nœuds pointant
-- vers la déclaration de la variable ou de la fonction dans l\'AST. Ceci est
-- utile et nécessaire pour garantir l\'utilisation correcte des variables et
-- des fonctions par rapport à leurs types déclarés. A proprement parler, il ne
-- s\'agit donc pas à d\'un arbre mais d\'un graphe.
--
-- Plus de détails sur l\'article <http://en.wikipedia.org/wiki/GADT>.
module Language.Coda.GAST where

import qualified Data.Set as S

import Language.Coda.AST

type GAST = [GTopLevel]

data GTopLevel where
    GTopLevelVar :: GVar GVarTypeVar q a r      -> GTopLevel
    GTopLevelFun :: GFun a r -> GCompoundStmt r -> GTopLevel

type GIdent = CIdent

-- Types, variables et fonctions -----------------------------------------------

-- | Représente les informations de déclaration d\'une variable.
--
-- * /t/ permet de spécifier le type de variable (variable ou argument d\'une
-- fonction) ;
--
-- * /q/ est le qualificateur utilisé lors de la définition de la variable 
-- (const);
--
-- * /a/ spécifie les « arguments » de la variable, c\'est à dire les indices
-- nécessaires pour arriver au type scalaire à partir d\'un tableau ;
--
-- * /r/ est le type « scalaire » de la variable, c\'est à dire la valeur de la
-- variable lorsque l\'ensemble des indices ont été fournis.
--
-- Si /a/ est égal à /r/, alors la variable est un scalaire. Pour un tableau à 
-- une dimension, /a/ sera égal à @GInt -> /r/@.
data GVar t q a r where
    -- Déclaration sans initialisation. Impose que la variable ne soit pas
    -- déclarée constante.
    GVarDecl :: GIdent -> GTypeArray GQualFree a r    -> GVar GVarTypeVar q a r
    GVarDef  :: GIdent -> GTypeArray q a r -> GExpr a -> GVar GVarTypeVar q a r
    -- Déclare une variable à partir d\'un argument. Contrairement à une
    -- variable normale, la dernière dimension d'un tableau peut être omise.
    GVarArg  :: GIdent -> GTypeArg q a r              -> GVar GVarTypeArg q a r

data GVarTypeVar
data GVarTypeArg

-- | Représente les informations de déclaration d\'une fonction.
--
-- Si /a/ est égal à /r/, alors la fonction est complètement appliquée.
-- Voir 'GFun' pour la sémantique complète de /a/ et /r/.
data GFun a r where
    GFun       :: GIdent -> GType b -> GFun b b
    GFunVoid   :: GIdent            -> GFun () ()
    -- Argument anonyme d'une fonction.
    GFunArg    :: GTypeArg q a r2 -> GFun b r1
                                    -> GFun (GTypeArg q a r2 -> b) r1
    -- Argument associé à un identifieur.
    GFunVarArg :: GVar GVarTypeArg q a r2 -> GFun b r
                                    -> GFun (GTypeArg q a r2 -> b) r1

type GInt  = CInt
type GBool = CBool

data GType a where
    GInt  :: GType GInt
    GBool :: GType GBool

-- | Utilise deux types fantômes pour annoter les types d'un qualificateur.
-- Permet d\'assurer qu\'une assignation ne peut se faire sur une constante,
-- par exemple.
data GQualType q a where
    GQualFree  :: GType a -> GQualType GQualFree  a
    GQualConst :: GType a -> GQualType GQualConst a

data GQualFree
data GQualConst

-- | Permet de définir un type de variable ou de tableau.
data GTypeArray q a r where
    GTypeArrayPrim :: GQualType q a            -> GTypeArray q a           a
    GTypeArray     :: GTypeArray q a r -> GInt -> GTypeArray q (GInt -> a) r

-- | Permet de définir un type d\'un argument d'une fonction. Étend 'GTypeArray'
-- de telle manière que la taille de la dernière dimension du tableau puisse 
-- être omise.
data GTypeArg q a r where
    GTypeArg              :: GTypeArray q a r -> GTypeArg q a           r
    -- Déclaration implicite de la dernière dimension du tableau.
    GTypeArgArrayImplicit :: GTypeArray q a r -> GTypeArg q (GInt -> a) r

-- Instructions et expressions -------------------------------------------------

-- | Défini un bloc de plusieurs instructions. Le type de retour de la
-- dernière instruction détermine le type du bloc.
data GCompoundStmt a where
    -- Défini un bloc vide. Pas de valeur de retour.
    GCompoundStmtFirst :: GCompoundStmt ()
    GCompoundStmtLast  :: GCompoundStmt a -> GStmt b -> GCompoundStmt b

data GStmt a where
    GExpr   :: GExpr a                           -> GStmt a
    GDecl   :: GVar GVarTypeVar q a r            -> GStmt ()
    -- Impose que l\'expression de gauche d\'une assignation ne soit pas
    -- constante et qu\'elle soit scalaire.
    GAssign :: GVarExpr GQualFree a a -> GExpr a -> GStmt ()
    GIf     :: GExpr GBool -> GCompoundStmt a
            -> Maybe (GCompoundStmt a)           -> GStmt a
    GWhile  :: GExpr GBool -> GCompoundStmt a    -> GStmt ()

data GExpr a where
    -- Appel une fonction. Impose l'application complète.
    GCall      :: GCall a a                        -> GExpr a
    -- Référence une variable. Impose l'application complète des indices.
    GVarExpr   :: GVarExpr q a a                   -> GExpr a
    GLitteral  :: GLitteral a                      -> GExpr a
    GBinOp     :: GBinOp e r -> GExpr e -> GExpr e -> GExpr r

-- | Types des opérateurs binaires. /e/ est le type des termes et /r/ le type de
-- retour.
data GBinOp e r where
    GAnd   :: GBinOp GBool GBool
    GOr    :: GBinOp GBool GBool
    GEq    :: GBinOp GInt  GBool
    GNEq   :: GBinOp GInt  GBool
    GLt    :: GBinOp GInt  GBool
    GGt    :: GBinOp GInt  GBool
    GLtEq  :: GBinOp GInt  GBool
    GGtEq  :: GBinOp GInt  GBool
    GAdd   :: GBinOp GInt  GInt
    GSub   :: GBinOp GInt  GInt
    GMult  :: GBinOp GInt  GInt
    GDiv   :: GBinOp GInt  GInt
    GMod   :: GBinOp GInt  GInt

-- | L'appel est complètement appliqué si /a/ est égal à /r/.
data GCall a r where
    GCallNoArg :: GFun a r                   -> GCall a r
    -- Applique la fonction à son premier argument.
    GCallArg   :: GExpr a -> GFun (a -> b) r -> GCall b r

-- | L'expression est scalaire si /a/ est égal à /r/.
data GVarExpr q a r where
    GVarExprPrim  :: GVar t q a r                           -> GVarExpr q a r
    GVarExprArray :: GVarExpr q (GInt -> a) r -> GExpr GInt -> GVarExpr q a r

data GLitteral a where
    GLitteralInt  :: GInt  -> GLitteral GInt
    GLitteralBool :: GBool -> GLitteral GBool
