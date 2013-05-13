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
import Data.Text (Text, unpack)

newtype AST = AST { astFuns :: [CFun] }
    deriving (Show)

newtype CIdent = CIdent { ciIdent :: Text }
    deriving (Eq, Ord)

instance Show CIdent where
    show (CIdent txt) = unpack txt

-- Variables et fonctions ------------------------------------------------------

data CFun = CFun (Maybe CType) CIdent CArgs (Maybe CCompoundStmt)
    deriving (Show)

data CVar = CVar CTypeArray CIdent
    deriving (Show)

-- Types -----------------------------------------------------------------------

data CType = CInt | CBool deriving (Eq)

instance Show CType where
    show CInt  = "int"
    show CBool = "bool"

data CTypeArray = CTypeArray CQual CType CDims
    deriving (Show, Eq)

-- | Si la variable est un tableau, contient le nombre de dimensions ainsi que
-- les tailles des n-1 dernières dimensions du tableau (la taille de la première
-- dimension est inutile pour calculer le padding d\'une cellule).
data CDims = CScalar | CArray Int [CInt]
    deriving (Show)

data CQual = CQualFree | CQualConst deriving (Show, Eq)

newtype CArgs = CArgs { caArgs :: [CArg] }
    deriving (Show)

-- | Encode un argument avec un nom de variable associée ou non.
data CArg = CVarArg CVar | CAnonArg CTypeArray
    deriving (Show)

getArgType :: CArg -> CTypeArray
getArgType (CVarArg (CVar t _)) = t
getArgType (CAnonArg t)         = t

-- Instances utilisées pour comparer la stricte égalité entre deux types.

instance Eq CDims where
    CScalar       == CScalar         = True
    CArray n dims == CArray n' dims' = n == n' && take n dims == take n' dims'
    _             == _               = False

instance Eq CArgs where
    (CArgs (x:xs)) == (CArgs (y:ys)) = x == y && CArgs xs == CArgs ys
    (CArgs [])     == (CArgs [])     = True
    _              == _              = False

instance Eq CArg where
    (==) = (==) `on` getArgType

-- Instructions et expressions -------------------------------------------------

data CCompoundStmt = CCompoundStmt {
      csStmts :: [CStmt]
    , csReturn :: Bool -- ^ Définit si le bloc retourne systématiquement.
    } deriving (Show)

data CStmt = CDecl CVarDecl
           | CAssign CVarExpr CExpr
           | CReturn (Maybe (CExpr, CType))
           | CExpr CExpr
           | CIf CExpr CCompoundStmt (Maybe CCompoundStmt)
           | CWhile CExpr CCompoundStmt
    deriving (Show)

-- | Contient la déclaration d\'une variable, le nombre d'\élements de la 
-- variable (1 si un scalaire) et une éventuelle valeur initiale.
data CVarDecl = CVarDecl CVar CInt (Maybe CExpr)
    deriving (Show)

data CExpr = CCall CFun [CExpr]
           | CVariable CVarExpr
           | CLitteral CLitteral
           | CBinOp CBinOp CExpr CExpr
    deriving (Show)

data CBinOp = CAnd | COr
            | CEq | CNEq | CLt | CGt | CLtEq | CGtEq
            | CAdd | CSub | CMult | CDiv | CMod

instance Show CBinOp where
    show CAnd  = "&&"
    show COr   = "||"
    show CEq   = "=="
    show CNEq  = "!="
    show CLt   = "<"
    show CGt   = ">"
    show CLtEq = "<="
    show CGtEq = ">="
    show CAdd  = "+"
    show CSub  = "-"
    show CMult = "*"
    show CDiv  = "/"
    show CMod  = "%"

data CVarExpr = CVarExpr CVar CDims [CExpr]
    deriving (Show)

type CBool = Bool
type CInt  = Int64

data CLitteral = CLitteralInt CInt | CLitteralBool CBool
    deriving (Show)
