-- | Définit les types liés à la définition de l'arbre syntaxique.
module Language.Coda.AST where

import Data.Int (Int64)
import Data.Text (Text)

type AST = [CTopLevel]

data CTopLevel = CTopLevelVar CVar | CTopLevelFun CFun
    deriving (Show, Eq)

data CVar = CVar (Maybe CTypeQual) CTypeArray CIdent (Maybe CExpr)
    deriving (Show, Eq)

data CFun = CFun (Maybe CType) CIdent [CArgument] (Maybe CCompoundStmt)
    deriving (Show, Eq)

data CType = CInt | CBool deriving (Show, Eq)

data CTypeArray = CTypeArray CType [CInt]
    deriving (Show, Eq)

-- | Contient le type d'un argument. Le boolen indique si la dernière dimension
-- du type, si c'est un tableau, est implicite.
data CTypeArg = CTypeArg CTypeArray Bool
    deriving (Show, Eq)

data CTypeQual = CConst deriving (Show, Eq)

data CArgument = CArgument (Maybe CTypeQual) CTypeArg (Maybe CIdent)
    deriving (Show, Eq)

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

type CIdent = Text

type CBool = Bool
type CInt  = Int64

data CLitteral = CLitteralInt CInt | CLitteralBool CBool
    deriving (Show, Eq)
