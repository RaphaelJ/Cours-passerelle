{-# LANGUAGE GADTs, RankNTypes #-}
-- | Définit les types liés à la définition de l'arbre syntaxique.
module Language.Coda.AST where

import Data.Text (Text)
import Data.Int (Int64)

type AST = [CTopLevelDecl]

data CTopLevelDecl = CTopLevelVariableDecl CVariableDecl
                   | CTopLevelFunctionDef CFunctionDef

data CVariableDecl where
    CVariableDecl :: Maybe CTypeQual -> CTypeArray a -> CIdentifier a
                  -> Maybe (CExpr a) -> CVariableDecl

data CFunctionDef where
    CFunctionDef :: CType a -> CIdentifier (b -> a) -> CArgument b
                 -> Maybe CCompoundStmt -> CFunctionDef

type CInt = Int64
type CBool = Bool

data CType a where
    CInt  :: CType CInt
    CBool :: CType CBool

data CTypeArray a where
    CPrim  :: CType a                    -> CTypeArray a
    CArray :: CTypeArray a -> Maybe CInt -> CTypeArray (CInt -> a)

data CTypeQual = CConst

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
    CIdentifier :: Text -> CTypeArray a -> CIdentifier a

data CLitteral a where
    CLitteralInt  :: CInt  -> CLitteral CInt
    CLitteralBool :: CBool -> CLitteral CBool
