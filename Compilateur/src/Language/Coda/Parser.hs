-- | Définit la fonction 'parse' qui génère un AST à partir d'un flux de
-- caractères (Lazy Text).
module Language.Coda.Parser (parser) where

import Control.Applicative ((<$>), (<*>), (<|>), (<*))
import qualified Data.Text as T
import Text.Parsec (
      (<?>), alphaNum, between, char, digit, eof, letter, many, many1, option
    , optionMaybe, space, spaces, string, try
    )
import Text.Parsec.Text.Lazy (Parser)

import Language.Coda.AST

type CodaParser = Parser

parser :: CodaParser AST
parser = do
    spaces
    (eof >> return []) <|> declaration
  where
    declaration = (:) <$> (    try (CTopLevelVariableDecl <$> variableDecl)
                           <|> try (CTopLevelFunctionDef  <$> functionDecl))
                      <*> parser

variableDecl :: CodaParser CVariableDecl
variableDecl = do
    qual <- typeQual <* spaces1
    spec <- typeArraySpec <* spaces1
    ident <- identifier <* spaces
    def <- optionMaybe (char '=' >> spaces >> expr <* spaces)
    _ <- char ';'
    return $ CVariableDecl qual spec ident def

functionDecl :: CodaParser CFunctionDef
functionDecl = fail "Not implemented"

typeQual :: CodaParser (Maybe CTypeQual)
typeQual = optionMaybe $ try $ string "const" >> return CConst

typeSpec :: CodaParser CType
typeSpec =     try (string "int"  >> return CInt)
           <|> try (string "bool" >> return CBool)

typeArraySpec :: CodaParser CTypeArray
typeArraySpec =
    CTypeArray <$> (typeSpec <* spaces) <*> subscripts
  where
    subscripts =
        option [] (try $ (:) <$> subscript integerLitteral
                             <*> subscripts)

expr, andExpr, comparisonExpr, numericExpr, multiplicativeExpr, valueExpr
    :: CodaParser CExpr
expr =     try (binaryExpr COr expr (string "||") andExpr)
       <|> try andExpr

andExpr =     try (binaryExpr CAnd andExpr (string "&&") comparisonExpr)
          <|> try comparisonExpr

comparisonExpr =
        try (binaryExpr CEq   comparisonExpr (string "==") numericExpr)
    <|> try (binaryExpr CNEq  comparisonExpr (string "!=") numericExpr)
    <|> try (binaryExpr CLt   comparisonExpr (char '<')    numericExpr)
    <|> try (binaryExpr CGt   comparisonExpr (char '>')    numericExpr)
    <|> try (binaryExpr CLtEq comparisonExpr (string "<=") numericExpr)
    <|> try (binaryExpr CGtEq comparisonExpr (string ">=") numericExpr)
    <|> try numericExpr

numericExpr =
        try (binaryExpr CAdd numericExpr (char '+') multiplicativeExpr)
    <|> try (binaryExpr CSub numericExpr (char '-') multiplicativeExpr)
    <|> try multiplicativeExpr

multiplicativeExpr =
        try (binaryExpr CMult multiplicativeExpr (char '*') valueExpr)
    <|> try (binaryExpr CDiv  multiplicativeExpr (char '/') valueExpr)
    <|> try (binaryExpr CMod  multiplicativeExpr (char '%') valueExpr)
    <|> try valueExpr

valueExpr =     try (CAssignable <$> assignableExpr)
            <|> try (CCall <$> (identifier <* spaces) <*> callArgs)
            <|> try (between (char '(') (char ')') (spaces >> expr <* spaces))
            <|> try (CLitteral <$> litteral)

assignableExpr :: CodaParser CAssignableExpr
assignableExpr =
    CAssignableExpr <$> identifier <*> subscripts
  where
    subscripts = option [] (try $ (:) <$> subscript expr <*> subscripts)

callArgs :: CodaParser [CExpr]
callArgs =
    between (char '(') (char ')') exprList
  where
    exprList = do
        spaces
        (:) <$> (expr <* spaces)
            <*> (    (char ',' >> exprList)
                 <|> return [])

litteral :: CodaParser CLitteral
litteral =     (CLitteralInt <$> integerLitteral)
           <|> (CLitteralBool <$> boolLitteral)

integerLitteral :: CodaParser CInt
integerLitteral = read <$> many1 digit
                       <?> "integer litteral"

boolLitteral :: CodaParser CBool
boolLitteral =     (string "true"  >> return True)
               <|> (string "false" >> return False)

identifier :: CodaParser T.Text
identifier =     (T.pack <$> ((:) <$> letter <*> many alphaNum))
             <?> "identifier"

-- | Parse et construit une expression à partir d'un opérateur binaire
-- associatif sur sa gauche. Les trois parseurs permettent de parser les deux
-- expressions à gauche et à droite de l'opérateur et l'opérateur lui-même.
binaryExpr :: CBinOp -> CodaParser CExpr -> CodaParser a -> CodaParser CExpr
           -> CodaParser CExpr
binaryExpr op left opParser right =
    CBinOp op <$> (left <* spaces <* opParser) <*> (spaces >> right)

-- | Parse une expression de définition ou de déréférencement d'une dimension
-- d'un tableau. Le parseur 'inner' parse le contenu entre [ et ]. Ignore les
-- espaces internes.
subscript :: CodaParser a -> CodaParser a
subscript inner = between (char '[')  (char ']') (spaces >> inner <* spaces)

-- | Parse 1 à N caractères d'espacement.
spaces1 :: CodaParser ()
spaces1 = space >> spaces
