import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Language.Coda.Parser (parse)

main = TL.interact (TL.pack . showAST . parse "stdin")
  where
    showAST (Left err)  = show err
    showAST (Right ast) = show $ length ast

