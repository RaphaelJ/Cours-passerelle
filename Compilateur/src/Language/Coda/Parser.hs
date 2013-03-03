-- | Définit la fonction 'parse' qui génère un AST à partir d'un flux de
-- caractères (Lazy Text).
module Language.Coda.Parser (parse) where

import Control.Applicative
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Parsec (
      SourceName, ParseError, (<?>)
    , optionMaybe, runParser, optionMaybestring, try
    )
import Text.Parsec.Text.Lazy (GenParser)

import Language.Coda.AST

data State = State { stIdentifiers :: M.Map T.Text Definition
    }

type CodaParser = GenParser State

parse :: SourceName -> TL.Text -> Either ParseError AST
parse = runParser parser (State M.empty)

parser :: CodaParser AST
parser = many do
    decl <- try variableDecl <|> try functionDecl
                             <?> "a top level variable or function definition."
    return decl

variableDecl :: CodaParser CTopLevelDecl
variableDecl = do
    qual <- typeQual
    varType <- typeArraySpec
    ident <- identifier
    
    let ident = CIdentifier 
    return (CVariableDecl typeQual varType 

functionDecl :: CodaParser CTopLevelDecl
functionDecl = CTopLevelFunctionDef

typeQual :: CodaParser (Maybe CTypeQual)
typeQual = optionMaybe $ try $ spaces >> string "const" >> return CConst

typeSpec :: CodaParser (CType a)
typeSpec = do
    spaces
    try (string "int" >> return CInt) <|> try (string "bool" >> return CBool)
                                      <?> "a type."

typeArraySpec :: CodaParser (CTypeArray a)