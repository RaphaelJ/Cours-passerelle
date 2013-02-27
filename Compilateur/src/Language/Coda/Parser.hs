-- | Définit la fonction 'parse' qui génère un AST à partir d'un flux de
-- caractères (Lazy Text).
module Language.Coda.Parser (parse) where

import Text.Parsec.Text.Lazy (Parser)