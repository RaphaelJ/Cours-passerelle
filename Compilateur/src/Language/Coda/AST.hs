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

-- | Contient la déclaration d\'une variable, les tailles des n dimensions de la
-- variable et une éventuelle valeur initiale.
data CVarDecl = CVarDecl CVar [Int] (Maybe CExpr)
    deriving (Show, Eq)

data CFun = CFun (Maybe CType) CIdent CArguments (Maybe CCompoundStmt)
    deriving (Show, Eq)

-- Types -----------------------------------------------------------------------

data CType = CInt | CBool deriving (Show, Eq)

data CTypeArray = CTypeArray CQual CType CDims
    deriving (Show, Eq)

-- | Si la variable est un tableau, contient le nombre de dimensions ainsi que
-- les tailles des n-1 dernières dimensions du tableau (la taille de la première
-- dimension est inutile pour calculer le padding d\'une cellule).
data CDims = CScalar | CArray Int [Int]
    deriving (Show)

data CQual = CQualFree | CQualConst deriving (Show, Eq)

newtype CArguments = CArguments [CArgument]
    deriving (Show)

-- | Encode un argument avec un nom de variable associée ou non.
data CArgument = CVarArgument CVar | CAnonArgument CTypeArray
    deriving (Show)

instance Eq CDims where
    CScalar       == CScalar         = True
    CArray n dims == CArray n' dims' = n == n' && take n dims == take n' dims'
    _             == _               = False

instance Eq CArguments where
    (CArguments (x:xs)) == (CArguments (y:ys)) =
        x == y && CArguments xs == CArguments ys
    (CArguments [])     == (CArguments [])     = True
    _                   == _                   = False

instance Eq CArgument where
    a == b = getType a == getType b
  where
    getType (CVarArgument (CVar t _)) = t
    getType (CAnonArgument t)         = t

-- Instructions et expressions -------------------------------------------------

type CCompoundStmt = [CStmt]

data CStmt = CDecl CVarDecl
           | CAssign CVarExpr CExpr
           | CReturn (Maybe CExpr)
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
