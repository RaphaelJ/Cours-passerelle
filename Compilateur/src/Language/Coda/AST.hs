-- | Définit les types liés à la définition de l\'arbre syntaxique.
--
-- Cet arbre est généré par le 'Parser'. Son typage et sa cohérence ne sont pas
-- fortement assurés, contrairement au 'GAST' produit par le vérificateur
-- sémantique. Toutes les instances de cet 'AST' ne sont donc pas nécessairement
-- valides ni exécutables, contrairement au 'GAST'.
--
-- Étant le résultat direct du parseur, il s\'agit d\'une transcription
-- quasiment à l\'identique de la grammaire EBNF du langage source.
--
-- Tous les noms des déclarations de ce fichier sont précédées d\'un C (pour
-- Coda).
module Language.Coda.AST where

import Data.Int (Int64)
import Data.Text (Text)

type AST = [CTopLevel]

data CTopLevel = CTopLevelVar CVar | CTopLevelFun CFun
    deriving (Show, Eq)

type CIdent = Text

-- Types, variables et fonctions -----------------------------------------------

data CVar = CVar CTypeArray CIdent (Maybe CExpr)
    deriving (Show, Eq)

data CFun = CFun (Maybe CType) CIdent [CArgument] (Maybe CCompoundStmt)
    deriving (Show, Eq)

-- | Types primitifs.
data CType = CInt | CBool deriving (Show, Eq)

-- | Permet de définir un type de variable ou de tableau, associé à un
-- qualificateur.
data CTypeArray = CTypeArray CQual CType [CInt]
    deriving (Show, Eq)

data CQual = CQualFree | CQualConst deriving (Show, Eq)

-- | Contient la déclaration d\'un argument. Le booléen indique si la dernière
-- dimension du type, dans le cas d\'un tableau, est implicite.
data CArgument = CArgument CTypeArray Bool (Maybe CIdent)
    deriving (Show, Eq)

-- Instructions et expressions -------------------------------------------------

type CCompoundStmt = [CStmt]

data CStmt = CDecl CVar
           | CAssign CVarExpr CExpr
           | CExpr CExpr
           | CIf CExpr CCompoundStmt (Maybe CCompoundStmt)
           | CWhile CExpr CCompoundStmt
    deriving (Show, Eq)

data CExpr = CCall CIdent [CExpr]
           | CVariable CVarExpr
           | CLitteral CLitteral
           | CBinOp CBinOp CExpr CExpr
    deriving (Show, Eq)

data CBinOp = CAnd | COr
            | CEq | CNEq | CLt | CGt | CLtEq | CGtEq
            | CAdd | CSub | CMult | CDiv | CMod
    deriving (Show, Eq)

data CVarExpr = CVarExpr CIdent [CExpr]
    deriving (Show, Eq)

type CBool = Bool
type CInt  = Int64

data CLitteral = CLitteralInt CInt | CLitteralBool CBool
    deriving (Show, Eq)
