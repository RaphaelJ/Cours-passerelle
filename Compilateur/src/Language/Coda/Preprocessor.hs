-- | Définit un préprocesseur simple qui supprime les commentaires du code
-- source.
module Language.Coda.Preprocessor (preprocess) where

import qualified Data.Text.Lazy as TL

-- | Préprocesse le code source pour y supprimer les commentaires.
preprocess :: TL.Text -> TL.Text
preprocess = TL.unlines . map (TL.takeWhile (/= '#')) . TL.lines
