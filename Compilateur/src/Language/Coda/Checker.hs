{-# LANGUAGE GADTs #-}
-- | Définit le vérificateur sémantique et de typage.
module Language.Coda.Checker (CheckerError, check) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error (ErrorT)
import Control.Monad.State (State, evalState, get)
import Data.List
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
    GVarDyn :: GVar t q a r -> GVarDyn

data GFunDyn where
    GFunDeclDyn :: GFun a r -> GFunDyn
    GFunDefDyn  :: GFun a r -> GFunDyn

data CheckerState = CheckerState {
      csVars :: M.Map GIdent GVarDyn, csFuns :: M.Map GIdent GFunDyn
    }

-- | Vérifie la validité d'un arbre syntaxique non typé et retourne l'arbre
-- typé correspondant.
check :: AST -> Either CheckerError GAST
check = undefined
-- check ast = runChecker initState (topLevel ast)
--   where
--     initState = CheckerState M.empty M.empty
-- 
--     runChecker state = flip evalState state . runErrorT
-- 
--     topLevel [] = return []
--     topLevel (CTopLevelVar v : xs) = (:) <$> var v <*> topLevel xs
--     topLevel (CTopLevelFun f : xs) =
--         let next = topLevel xs
--         in fun f >>= maybe next ((<$> next) . (:))
-- 
--     var (CVar qual ret ident expr) = do
--         st <- get
--         case ident `M.lookup` csVars st of
--             Just _  -> throwError "Variable already defined."
--             Nothing -> 
-- 
--     fun (CFun ret ident args (Just stmts)) = do
--         st <- get
--         case ident `M.lookup` csFuns st of
--             Just (GFunIdentDeclDyn declIdent) |  ->
--                 
--             Just (GFunIdentDefDyn _) -> throwError "Multiple function definition."
--             Nothing -> 
--     fun (CFun type ident args Nothing)
--         return Nothing

-- funType :: CIdent -> CType -> [CArgument] -> GFun a r
-- funType ident t =
--     foldl' step prim . reverse
--  where
--     prim = case t of
--         Just CInt  -> GFun ident GInt
--         Just CBool -> GFun ident GBool
--         Nothing    -> GFunVoid ident
-- 
--     step acc (CArgument t hasImplDim _) = GFunArg (typeArg t hasImplDim) acc
-- --     step acc (CArgument CTypeArray hasImplDim (Just ident)) =
-- --         GFunVarArg acc
-- 
--     typeArg t False = GTypeArg              (arrayType t)
--     typeArg t True  = GTypeArgArrayImplicit (arrayType t)

data GTypeDyn where
    GTypeDyn :: GType a -> GTypeDyn

data GTypeArrayDyn where
    GTypeArrayDyn :: GTypeArray q a r -> GTypeArrayDyn

arrayType :: CTypeArray -> GTypeArrayDyn
arrayType (CTypeArray qual t dims) =
    foldl' step prim $ reverse dims
  where
    step (GTypeArrayDyn nested) dim =
        GTypeArrayDyn $ GTypeArray nested dim

--     GTypeDyn t' = 

    prim = case qual of
        CQualFree  -> GTypeArrayDyn $ GTypeArrayPrim $ GQualFree $ case primType t of GTypeDyn t' -> t'
        CQualConst -> GTypeArrayDyn $ GTypeArrayPrim $ GQualConst $ case primType t of GTypeDyn t' -> t'

primType :: CType -> GTypeDyn
primType CInt  = GTypeDyn GInt
primType CBool = GTypeDyn GBool