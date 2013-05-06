{-# LANGUAGE OverloadedStrings #-}
-- | Définit le générateur de code qui génère le code IR d\'LLVM à partir de
-- l\'arbre syntaxique du programme.
module Language.Coda.CodeGen (genCode, codeGen) where

import Control.Monad.State (State, get, put)
import Control.Monad.Writer (WriterT, tell)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, toLazyText)

import Language.Code.AST

type CodeGen = WriterT TL.Builder (State CodeGenState)

data CodeGenState = CodeGenState

genCode :: AST -> TL.Text
genCode = TL.unlines . map (TL.takeWhile (/= '#')) . TL.lines

CodeGen a -> 
runCodeGen = 

codeGen :: AST -> CodeGen ()