import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Language.Coda.Parser (parse)
import Language.Coda.Preprocessor (preprocess)

main :: IO ()
main = do
    TL.interact (TL.pack . show . parse "stdin" . preprocess)
    putStrLn ""
