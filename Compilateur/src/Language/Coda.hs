import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Language.Coda.Parser (parse)
import Language.Coda.Preprocessor (preprocess)

main :: IO ()
main = do
--     TL.interact (TL.pack . show . parse "stdin")
--     TL.interact (preprocess)
    (preprocess . TL.replicate 100000) `fmap` TL.readFile "quicksort.coda" >>= TL.writeFile "/dev/null"
--     (TL.unwords . TL.lines . TL.replicate 100000) `fmap` TL.readFile "quicksort.coda" >>= TL.writeFile "/dev/null"
    putStrLn ""
