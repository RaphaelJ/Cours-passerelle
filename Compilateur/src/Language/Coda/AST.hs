{-# LANGUAGE: GADTs #-}
-- | Définit les types liés à la définition de l'arbre syntaxique.
module Language.Coda.AST () where

import Data.Text

data Variable = Variable Type Text
    Declaration = DVariable Type Text

data CType = CInt | CFloat | CChar | CArray CType (Maybe Int)
    deriving (Show, Eq)

type CCondition = CExpr Bool