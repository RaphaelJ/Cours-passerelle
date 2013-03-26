{-# LANGUAGE GADTs #-}
-- | Définit les types liés à la définition de l\'arbre syntaxique typé.
--
-- L\'utilisation des types algébriques généralisés permet de garantir que
-- chaque instance de l\'arbre syntaxique sera correctement typée (en d\'autres
-- mots, que le vérificateur de types ne peut générer un arbre syntaxique qui
-- ne respecterait pas les règles de typage définies ci-dessus). Cette assertion
-- est garantie par le vérificateur de types de Haskell, qui n\'est jamais
-- qu\'un vérificateur de preuves étendu (voir
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

import Language.Coda.AST

type GAST = [GTopLevelDecl]

data GTopLevelDecl = GTopLevelVariableDecl GVariableDecl
                   | GTopLevelFunctionDef  GFunctionDef

data GVariableDecl where
    GVariableDecl :: Maybe GTypeQual -> GTypeArray a -> GIdentifier a
                  -> Maybe (GExpr a) -> GVariableDecl

data GFunctionDef where
    GFunctionDef :: GType a -> GIdentifier (b -> a) -> GArgument b
                 -> Maybe (GCompoundStmt a) -> GFunctionDef

type GInt  = CInt
type GBool = CBool

data GType a where
    GInt  :: GType CInt
    GBool :: GType CBool

data GTypeArray a where
    GPrim     :: GType a                    -> GTypeArray a
    GArray    :: GTypeArray a -> Maybe CInt -> GTypeArray (CInt -> a)

data GTypeArrayArg a where
    GTypeArray :: 
    GTypeArray :: 
    GArrayArg :: GTypeArray a               -> GTypeArray (CInt -> a)

type GTypeQual = CTypeQual

data CArgument a where
    CArgument     :: Maybe CTypeQual -> CTypeArray a -> Maybe (CIdentifier a)
                  -> CArgument b -> CArgument (a -> b)
    CArgumentVoid :: CArgument ()

type CCompoundStmt = [CStmt]

data CStmt where
    CExpr   :: CExpr a -> CStmt
    CDecl   :: CVariableDecl -> CStmt
    CAssign :: CAssignableExpr a -> CExpr a -> CStmt
    CIf     :: CExpr CBool -> CCompoundStmt -> Maybe CCompoundStmt -> CStmt
    CWhite  :: CExpr CBool -> CCompoundStmt -> CStmt
    CReturn :: CExpr a -> CStmt

data CExpr a where
    CLitteral   :: CLitteral a                             -> CExpr a
    CAssignable :: CAssignableExpr a                       -> CExpr a
    CCall       :: CIdentifier (a -> r) -> CCallArgument a -> CExpr r
    CBinOp      :: CBinOp e r -> CExpr e -> CExpr e        -> CExpr r

data CBoolLiteral = CTrue | CFalse deriving (Show, Eq)

data CBinOp e r where
    CAnd   :: CBinOp CBool CBool
    COr    :: CBinOp CBool CBool
    CEq    :: CBinOp CInt  CBool
    CNEq   :: CBinOp CInt  CBool
    CLt    :: CBinOp CInt  CBool
    CGt    :: CBinOp CInt  CBool
    CLtEq  :: CBinOp CInt  CBool
    CGtEq  :: CBinOp CInt  CBool
    CAdd   :: CBinOp CInt  CInt
    CSub   :: CBinOp CInt  CInt
    CMult  :: CBinOp CInt  CInt
    CDiv   :: CBinOp CInt  CInt
    CMod   :: CBinOp CInt  CInt

data CCallArgument a where
    CCallArgument     :: CExpr a -> CCallArgument b -> CCallArgument (a -> b)
    CCallArgumentVoid :: CCallArgument ()

data CAssignableExpr a where
    CAssignableArray      :: CIdentifier (CInt -> a) -> CExpr CInt
                          -> CAssignableExpr a
    CAssignableIdentifier :: CIdentifier a -> CAssignableExpr a

data CIdentifier a where
    CIdentifier :: a -> CIdentifier a

data CLitteral a where
    CLitteralInt  :: CInt  -> CLitteral CInt
    CLitteralBool :: CBool -> CLitteral CBool
