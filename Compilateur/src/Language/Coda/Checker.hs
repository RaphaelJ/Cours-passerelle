{-# LANGUAGE GADTs #-}
-- | Définit le vérificateur sémantique et de typage.
module Language.Coda.Checker (check) where

import Control.Monad.State.Lazy (State (..), evalState)
import qualified Data.Map as M

import Language.Coda.AST
import Language.Coda.GAST

data GVarIdentDyn where
    GVarIdentDyn :: GVarIdent q a -> GVarIdentDyn

data GFunIdentDyn where
    GFunIdentDyn :: GFunIdent a -> GFunIdentDyn

data CheckerState = CheckerState {
      csVars :: M.Map CIdent GVarIdentDyn
    , csFuns :: M.Map CIdent GFunIdentDyn
    }

-- | Vérifie la validité d'un arbre syntaxique non typé et retourne l'arbre
-- typé correspondant.
check :: AST -> GAST
check ast = evalState initState $ do
    return []
  where
    initState = CheckerState M.empty M.empty
