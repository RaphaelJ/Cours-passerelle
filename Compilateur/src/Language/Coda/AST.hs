-- | Définit les types liés à la définition de l\'arbre syntaxique.
--
-- Cet arbre est généré par le 'Parser'. Son typage et sa cohérence ne sont pas
-- fortement assurés, contrairement au 'TAST' produit par le vérificateur
-- sémantique.
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

data CTopLevel = CTopLevelVar CVarDecl | CTopLevelFun CFun
    deriving (Show, Eq)

type CIdent = Text

-- Variables et fonctions ------------------------------------------------------

data CVar = CVar CTypeArray CIdent
    deriving (Show, Eq)

data CVarDecl = CVarDecl CVar (Maybe CExpr)
    deriving (Show, Eq)

data CFun = CFun (Maybe CType) CIdent [CArgument] (Maybe CCompoundStmt)
    deriving (Show, Eq)

-- Types -----------------------------------------------------------------------

data CType = CInt | CBool deriving (Show, Eq)

-- | Permet de définir un type de variable ou d\'un tableau.
-- Si la variable est un tableau, contient le nombre de dimensions ainsi que les
-- tailles d\'au moins les n-1 premières dimensions du tableau.
data CTypeArray = CTypeArray CQual CType (Maybe (Int, [Int]))
    deriving (Show, Eq)

data CQual = CQualFree | CQualConst deriving (Show, Eq)

-- | Encode les arguments avec une variable associée ou non.
data CArgument = CVarArgument CVar | CAnonArgument CTypeArray
    deriving (Show, Eq)

-- Instructions et expressions -------------------------------------------------

type CCompoundStmt = [CStmt]

data CStmt = CDecl CVarDecl
           | CAssign CVarExpr CExpr
           | CReturn CExpr
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

data CVarExpr = CVarExpr CVar [CExpr]
    deriving (Show, Eq)

type CBool = Bool
type CInt  = Int64

data CLitteral = CLitteralInt CInt | CLitteralBool CBool
    deriving (Show, Eq)
