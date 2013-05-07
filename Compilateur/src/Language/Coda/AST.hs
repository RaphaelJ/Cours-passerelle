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
import Data.Function (on)
import Data.Text (Text)

newtype AST = AST [CFun]
    deriving (Show)

type CIdent = Text

-- Variables et fonctions ------------------------------------------------------

data CFun = CFun (Maybe CType) CIdent CArguments (Maybe CCompoundStmt)
    deriving (Show)

data CVar = CVar CTypeArray CIdent
    deriving (Show)

-- Types -----------------------------------------------------------------------

data CType = CInt | CBool deriving (Show, Eq)

data CTypeArray = CTypeArray CQual CType CDims
    deriving (Show, Eq)

-- | Si la variable est un tableau, contient le nombre de dimensions ainsi que
-- les tailles des n-1 dernières dimensions du tableau (la taille de la première
-- dimension est inutile pour calculer le padding d\'une cellule).
data CDims = CScalar | CArray Int [CInt]
    deriving (Show)

data CQual = CQualFree | CQualConst deriving (Show, Eq)

newtype CArguments = CArguments [CArgument]
    deriving (Show)

-- | Encode un argument avec un nom de variable associée ou non.
data CArgument = CVarArgument CVar | CAnonArgument CTypeArray
    deriving (Show)

getArgType :: CArgument -> CTypeArray
getArgType (CVarArgument (CVar t _)) = t
getArgType (CAnonArgument t)         = t

-- Instances utilisées pour comparer la stricte égalité entre deux types.

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
    (==) = (==) `on` getArgType

-- Instructions et expressions -------------------------------------------------

type CCompoundStmt = [CStmt]

-- | Contient la déclaration d\'une variable, le nombre d'\élements de la 
-- variable (1 si un scalaire) et une éventuelle valeur initiale.
data CVarDecl = CVarDecl CVar CInt (Maybe CExpr)
    deriving (Show)

data CStmt = CDecl CVarDecl
           | CAssign CVarExpr CExpr
           | CReturn (Maybe CExpr)
           | CExpr CExpr
           | CIf CExpr CCompoundStmt (Maybe CCompoundStmt)
           | CWhile CExpr CCompoundStmt
    deriving (Show)

data CExpr = CCall CFun [CExpr]
           | CVariable CVarExpr
           | CLitteral CLitteral
           | CBinOp CBinOp CExpr CExpr
    deriving (Show)

data CBinOp = CAnd | COr
            | CEq | CNEq | CLt | CGt | CLtEq | CGtEq
            | CAdd | CSub | CMult | CDiv | CMod
    deriving (Show)

data CVarExpr = CVarExpr CVar [CExpr]
    deriving (Show)

type CBool = Bool
type CInt  = Int64

data CLitteral = CLitteralInt CInt | CLitteralBool CBool
    deriving (Show)
