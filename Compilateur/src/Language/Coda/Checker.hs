{-# LANGUAGE GADTs #-}
-- | Définit le vérificateur sémantique et de typage.
module Language.Coda.Checker (check) where

import Control.Monad.State.Lazy (State (..), evalState, get)
import qualified Data.Map as M

import Language.Coda.AST
import Language.Coda.GAST

data GVarIdentDyn where
    GVarIdentDyn :: GVarIdent q a -> GVarIdentDyn

data GFunIdentDyn where
    GFunIdentDyn :: GFunIdent a -> GFunIdentDyn

data CheckerState = CheckerState {
      csVars :: M.Map GIdent GVarIdentDyn, csFuns :: M.Map GIdent GFunIdentDyn
    }

-- | Vérifie la validité d'un arbre syntaxique non typé et retourne l'arbre
-- typé correspondant.
check :: AST -> GAST
check ast = evalState (topLevel ast) initState
  where
    initState = CheckerState M.empty M.empty

    topLevel [] = return []
    topLevel (CTopLevelVar (CVar qual type ident expr) : xs) = do
        st <- get
        case ident `M.lookup` csVars st of
            Just _  -> fail "Variable already defined."
            Nothing -> 
    topLevel (CTopLevelFun (CFun type ident args expr) : xs) = do
        

