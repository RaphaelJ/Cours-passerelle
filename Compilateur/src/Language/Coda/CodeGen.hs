{-# LANGUAGE OverloadedStrings #-}
-- | Définit le générateur de code qui génère le code IR d\'LLVM à partir de
-- l\'arbre syntaxique du programme.
module Language.Coda.CCodeGen (genCode, codeGen) where

import Control.Monad.State (State, get, put, state)
import Data.Monoid ((<>))
import Control.Monad.Writer (WriterT, tell)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, toLazyText)

import Language.Code.AST

type CCodeGen = WriterT TL.Builder (State CCodeGenState)
-- type CCodeGen = State LLVMIdent

newtype LLVMIdent = LLVMIdent Int

type CCodeGenState = LLVMIdent

-- data CCodeGenState = CCodeGenState {
--       cgsNextIdent :: !LLVMIdent, cgs
--     }

genCode :: AST -> TL.Text
genCode = TL.unlines . map (TL.takeWhile (/= '#')) . TL.lines

CCodeGen a -> 
runCCodeGen = 

codeGen :: AST -> CCodeGen ()

-- Fonctions -------------------------------------------------------------------

cFun :: CFun -> CCodeGen Builder
cFun (CFun ret ident args mStmts) = do
    let ret' | Just t <- ret = cType t
             | otherwise     = "void"
        ident' = "@" <> ident
    args' <- 

    let fType = ret' <> " " <> ident' <> args'
    return $ case mStmts of
        Just stmts -> "define "  <> fType <> stmts'
        Nothing    -> "declare " <> fType

-- ; Declare the string constant as a global constant.
-- @.str = global [13 x i8] c"hello world\0A\00"
-- 
-- ; Definition of main function
-- define i32 @main() {   ; i32()*
--   ; Convert [13 x i8]* to i8  *...
--   %cast210 = getelementptr [13 x i8]* @.str, i64 0, i64 0
-- 
--   ; Call puts function to write out the string to stdout.
--   call i32 @puts(i8* %cast210)
--   ret i32 0
-- }
-- ; External declaration of the puts function
-- declare i32 @puts(i8*)


    

-- Types -----------------------------------------------------------------------

class Emitable a where
    emit :: a -> Builder

instance Emitable CType where
    emit CInt  = "i64"
    emit CBool = "i1"

class LLVMType a where
    emitType :: a -> Builder

instance LLVMType (LLVMIdent CInt) where
    emitType _ = "i64"

instance LLVMType (LLVMIdent CBool) where
    emitType _ = "i1"

cTypeArray :: CTypeArray -> Builder
cTypeArray =

cArguments 

-- Instructions et expressions -------------------------------------------------

data LLVMIdent = LLVMIdent Int

data LLVMLitteral = 

data LLVMValue = LLVMValueIdent LLVMIdent | LLVMValueLitteral LLVMLitteral

newtype LLVMOp = LLVMOp Builder

load :: CType -> LLVMIdent a -> LLVMIdent
load t v = add t (LLVMLitteral 0) (LLVMLitteral v)

add :: CType -> LLVMIdent -> LLVMIdent -> CCodeGen LLVMIdent
add t a b = LLVMOp $ "add" <> " " <> emit t <> " " <> emit a <> " " <> emit b

ret :: Maybe (CType, LLVMIdent) -> LLVMIdent

<- add (LLVMLitteral 10) (LLVMLitteral 20)

<- assign $ load 10

ret :: CType -> LLVMIdent -> LLVMOpCode

cLitteral :: CLitteral -> CCodeGen LLVMIdent
cLitteral (CLitteralInt i)      = load CInt  i
cLitteral (CLitteralBool True)  = load CBool 1
cLitteral (CLitteralBool False) = load CBool 1


-- Utilitaires -----------------------------------------------------------------

newIdent :: CCodeGen LLVMIdent
newIdent = state $ \i@(LLVMIdent v) -> (i, LLVMIdent (i + 1))

assign :: Builder -> 

tellLine :: T.Text -> CCodeGen ()
tellLine = tell . (`snoc` '\n')
