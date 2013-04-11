{-# LANGUAGE GADTs #-}
-- | Définit le vérificateur sémantique et de typage.
module Language.Coda.Checker (CheckerError, check) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error (ErrorT)
import Control.Monad.State (State, evalState, get)
import qualified Data.Map as M

import Language.Coda.AST
import Language.Coda.GAST

-- | Utilise deux niveaux de monades lors de l'exécution du checker.
--
-- 'ErrorT' récupère les erreurs sémantiques ;
-- 'State' maintient l'état des tables de symboles des variables et des
-- fonctions.
type Checker = ErrorT CheckerError (State CheckerState)

type CheckerError = String

data GVarDyn where
    GVarDyn :: GVar q a -> GVarDyn

data GFunIdentDyn where
    GFunIdentDeclDyn :: GFunIdent a -> GFunIdentDyn
    GFunIdentDefDyn  :: GFunIdent a -> GFunIdentDyn

data CheckerState = CheckerState {
      csVars :: M.Map GIdent GVarIdentDyn, csFuns :: M.Map GIdent GFunIdentDyn
    }

-- | Vérifie la validité d'un arbre syntaxique non typé et retourne l'arbre
-- typé correspondant.
check :: AST -> Either CheckerError GAST
check ast = runChecker initState (topLevel ast)
  where
    initState = CheckerState M.empty M.empty

    runChecker state = flip evalState state . runErrorT

    topLevel [] = return []
    topLevel (CTopLevelVar v : xs) = (:) <$> var v <*> topLevel xs
    topLevel (CTopLevelFun f : xs) =
        let next = topLevel xs
        in fun f >>= maybe next ((<$> next) . (:))

    var (CVar qual type ident expr) = do
        st <- get
        case ident `M.lookup` csVars st of
            Just _  -> throwError "Variable already defined."
            Nothing -> 

    fun (CFun type ident args (Just stmts)) = do
        st <- get
        case ident `M.lookup` csFuns st of
            Just (GFunIdentDeclDyn declIdent) |  ->
                
            Just (GFunIdentDefDyn _) -> throwError "Multiple function definition."
            Nothing -> 
    fun (CFun type ident args Nothing) 
        return Nothing

funType :: CType -> [CArgument] -> GFun a
funType type =
    foldl' step (prim type)
 where
    prim (Just CInt)  = GFunRet GInt
         (Just CBool) = GFunRet GBool
         Nothing      = GFunVoid

    step acc (CArgument qual (CTypeArrayArg CTypeArray Bool) CTypeArrayArg (Maybe CIdent)
    
    argType 