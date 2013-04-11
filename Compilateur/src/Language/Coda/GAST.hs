{-# LANGUAGE GADTs #-}
-- | Définit les types liés à la définition de l\'arbre syntaxique typé.
--
-- L\'utilisation des types algébriques généralisés permet de garantir que
-- chaque instance de l\'arbre syntaxique sera correctement typée (en d\'autres
-- mots, que le vérificateur de types ne peut générer un arbre syntaxique qui
-- ne respecterait pas les règles de typage définies ci-dessus). Cette assertion
-- est garantie par le vérificateur de types de Haskell, qui est un vérificateur
-- de preuves étendu (voir
-- <http://en.wikipedia.org/wiki/Curry–Howard_correspondence>).
--
-- Par exemple l\'expression suivante :
-- > int a = true;
-- Peut être représentée dans la version non généralisée de l\''AST' mais ne
-- saurait être représentée dans cette version typée.
--
-- De plus, le compilateur peut également garantir qu'il existe un chemin
-- d'exécution dans le générateur de code pour toute instance de cet AST,
-- quelque soit la combinaison des types de ce dernier.
--
-- Plus de détails sur l\'article de Wikipedia :
-- <http://en.wikipedia.org/wiki/GADT>
module Language.Coda.GAST where

import qualified Data.Set as S

import Language.Coda.AST

type GAST = [GTopLevel]

data GTopLevel where
    GTopLevelVar :: GVar q a -> GTopLevel
    GTopLevelFun :: GFun a   -> GTopLevel

type GIdent = CIdent

data GVar q a where
    -- | Déclaration sans initialisation. Impose que la variable ne soit pas
    -- déclarée constante.
    GVarDecl :: GIdent -> GTypeArray GTypeQualFree a -> GVar q a
    GVarDef  :: GIdent -> GTypeArray q a -> GExpr a  -> GVar q a
    GVarArg  :: GIdent -> GTypeArray q a             -> GVar q a

data GFun a where
    GFun       :: GIdent -> GType b       -> GFun b
    GFunVoid   :: GIdent                  -> GFun ()
    GFunArg    :: GTypeArg q a -> GFun b  -> GFun (GTypeArg q a -> b)
    GFunVarArg :: GVar q a -> GFun b      -> GFun (GTypeArg q a -> b)

type GInt  = CInt
type GBool = CBool

data GType a where
    GInt  :: GType CInt
    GBool :: GType CBool

data GQualifiedType q a = GQualifiedType (GTypeQual q) (GType a)

data GTypeQual a where
    GTypeQualFree  :: GTypeQual GTypeQualFree
    GTypeQualConst :: GTypeQual GTypeQualConst

data GTypeQualFree
data GTypeQualConst

data GTypeArray q a where
    GTypeArrayPrim :: GQualifiedType q a     -> GTypeArray q a
    GTypeArray     :: GTypeArray q a -> GInt -> GTypeArray q (GInt -> a)

data GTypeArg q a where
    GTypeArg              :: GTypeArray q a -> GTypeArg q a
    -- | Déclaration implicite de la dernière dimension du tableau.
    GTypeArgArrayImplicit :: GTypeArray q a -> GTypeArg q (GInt -> a)

data GCompoundStmt a where
    -- | Défini un bloc vide. Pas de valeur de retour.
    GCompoundStmtFirst :: GCompoundStmt ()
    -- | Défini un bloc de plusieurs instructions. Le type de retour de la
    -- dernière instruction détermine le type du bloc.
    GCompoundStmtLast  :: GCompoundStmt a -> GStmt b -> GCompoundStmt b

data GStmt a where
    GExpr   :: GExpr a                             -> GStmt a
    GDecl   :: GVar                                -> GStmt ()
    -- | Impose que l'expression de gauche d'une assignation ne soit pas
    -- constante.
    GAssign :: GVarExpr GTypeQualFree a -> GExpr a -> GStmt ()
    GIf     :: GExpr GBool -> GCompoundStmt a
            -> Maybe (GCompoundStmt a)             -> GStmt a
    GWhile  :: GExpr GBool -> GCompoundStmt a      -> GStmt ()

data GExpr a where
    GCall      :: GCall a                             -> GExpr r
    GVarExpr   :: GVarExpr q a                        -> GExpr a
    GLitteral  :: GLitteral a                         -> GExpr a
    GBinOp     :: GBinOp e r -> GExpr e -> GExpr e    -> GExpr r

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

data GCall a where
    GCallNoArg :: GFun a                    -> GCall a
    -- | Applique la fonction à son premier argument.
    GCallArg   :: GCall (a -> b) -> GExpr a -> GCall b

data GVarExpr q a where
    GVarExprPrim  :: GVar q a                             -> GVarExpr q a
    GVarExprArray :: GVarExpr q (GInt -> a) -> GExpr GInt -> GVarExpr q a

data GLitteral a where
    GLitteralInt  :: GInt  -> GLitteral GInt
    GLitteralBool :: GBool -> GLitteral GBool
