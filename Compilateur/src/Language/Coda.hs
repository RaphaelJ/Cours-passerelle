import Control.Applicative ((<$>))
import qualified Data.Text.Lazy.IO as TL
import System.IO (hPrint, stderr)

import Language.Coda.CodeGen (genCode)
import Language.Coda.Parser (parse)
import Language.Coda.Preprocessor (preprocess)

main :: IO Int
main = do
    eAST <- (parse "stdin" . preprocess) <$> TL.getContents
    case eAST of
        Left err     -> hPrint stderr err            >> return 1
        Right instrs -> TL.putStrLn (genCode instrs) >> return 0
