import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Text.Parsec (parse)

import Language.Coda.Parser (parser)

main :: IO ()
main = do
    TL.interact (TL.pack . show . parse parser "stdin")
    putStrLn ""
