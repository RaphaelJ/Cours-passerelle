import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Language.Coda.Parser (parse)

main :: IO ()
main = do
    TL.interact (TL.pack . show . parse "stdin")
    putStrLn ""
