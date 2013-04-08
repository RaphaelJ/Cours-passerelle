-- | Définit les types liés à la définition de l'arbre syntaxique.
module Language.Coda.AST where

import Data.Int (Int64)
import Data.Text (Text)

type AST = [CTopLevelDecl]

data CTopLevelDecl = CTopLevelVariableDecl CVariableDecl
                   | CTopLevelFunctionDef  CFunctionDef
    deriving (Show, Eq)

data CVariableDecl = CVariableDecl (Maybe CTypeQual) CTypeArray CIdentifier
                                   (Maybe CExpr)
    deriving (Show, Eq)

data CFunctionDecl = CFunctionDecl (Maybe CType) CIdentifier [CArgument]
                                   (Maybe CCompoundStmt)
    deriving (Show, Eq)

data CType = CInt | CBool deriving (Show, Eq)

data CTypeArray = CTypeArray CType [CInt]
    deriving (Show, Eq)

data CTypeArrayArg = CTypeArrayArg CTypeArray Bool
    deriving (Show, Eq)

data CTypeQual = CConst deriving (Show, Eq)

data CArgument = CArgument (Maybe CTypeQual) CTypeArrayArg (Maybe CIdentifier)
    deriving (Show, Eq)

type CCompoundStmt = [CStmt]

data CStmt = CExpr CExpr | CDecl CVariableDecl
           | CAssignation CAssignableExpr CExpr
           | CReturn CExpr
           | CIf CExpr CCompoundStmt (Maybe CCompoundStmt)
           | CWhile CExpr CCompoundStmt
    deriving (Show, Eq)

data CExpr = CCall CIdentifier [CExpr]
           | CLitteral CLitteral
           | CAssignable CAssignableExpr
           | CBinOp CBinOp CExpr CExpr
    deriving (Show, Eq)

data CBinOp = CAnd | COr
            | CEq | CNEq | CLt | CGt | CLtEq | CGtEq
            | CAdd | CSub | CMult | CDiv | CMod
    deriving (Show, Eq)

data CAssignableExpr = CAssignableExpr CIdentifier [CExpr]
    deriving (Show, Eq)

type CIdentifier = Text

type CBool = Bool
type CInt  = Int64

data CLitteral = CLitteralInt CInt | CLitteralBool CBool
    deriving (Show, Eq)
