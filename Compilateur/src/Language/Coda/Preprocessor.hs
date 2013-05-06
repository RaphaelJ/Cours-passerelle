{-# LANGUAGE OverloadedStrings #-}
module Language.Coda.Preprocessor (preprocess) where

import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (Builder, toLazyText, fromLazyText)
import Control.Arrow (second)
import Data.Monoid ((<>))

-- preprocess :: T.Text -> T.Text
-- preprocess = T.unlines . map preprocessor . T.lines
-- 
-- preprocessor :: T.Text -> T.Text
-- preprocessor = T.takeWhile (/= '#')

preprocess :: T.Text -> T.Text
preprocess = 
    toLazyText . preprocessor
  where
    preprocessor txt | T.null txt = fromLazyText txt
                     | otherwise  =
        let (before, after) = second (T.dropWhile (/= '\n')) $ T.break (== '#') txt
        in fromLazyText before <> preprocessor after

-- preprocess :: T.Text -> T.Text
-- preprocess = preprocessor
--   where
--     preprocessor t | T.null t  = t
--                    | otherwise =
--         let (l, t') = T.break (== '#') t
--             t'' = T.dropWhile (/= '\n') t'
--         in l `T.append` preprocessor t''
