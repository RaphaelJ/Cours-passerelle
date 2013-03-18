-- | Définit la fonction 'parse' qui génère un AST à partir d'un flux de
-- caractères (Lazy Text).
module Language.Coda.Parser (parser) where

import Control.Applicative ((<$>), (<*>), (<|>), (<*), (*>), pure)
import qualified Data.Text as T
import Text.Parsec (
      (<?>), alphaNum, between, char, digit, letter, many, many1, optionMaybe
    , sepBy, space, spaces, string, try
    )
import Text.Parsec.Text.Lazy (Parser)

import Language.Coda.AST

type CodaParser = Parser

parser :: CodaParser AST
parser =
    many (spaces >> declaration)
  where
    declaration =     try (CTopLevelVariableDecl <$> variableDecl)
                  <|> try (CTopLevelFunctionDef  <$> functionDecl)

-- Déclarations ----------------------------------------------------------------

variableDecl :: CodaParser CVariableDecl
variableDecl =
    CVariableDecl <$> typeQual
                  <*> typeArraySpec <* spaces1
                  <*> identifier <* spaces
                  <*> optionMaybe (char '=' *> spaces *> expr)
                  <*  tailingSep

functionDecl :: CodaParser CFunctionDef
functionDecl =
    CFunctionDef <$> optionMaybe (typeSpec <* spaces1)
                 <*> identifier <* spaces
                 <*> args <* spaces
                 <*> (    try (Just <$> compoundStmt)
                      <|> (char ';' >> return Nothing))
  where
    args = between (char '(' >> spaces) (spaces >> char ')')
                   (arg `sepBy` (spaces >> char ',' >> spaces))

    arg = CArgument <$> typeQual
                    <*> typeArrayArgSpec
                    <*> optionMaybe (try $ spaces1 *> identifier)

    typeArrayArgSpec =
        CTypeArrayArg <$> typeArraySpec
                      <*> (    (try $ subscript spaces >> return True)
                           <|> return False)

typeQual :: CodaParser (Maybe CTypeQual)
typeQual = optionMaybe $ try $ string "const" >> spaces1 >> return CConst

typeSpec :: CodaParser CType
typeSpec =     try (string "int"  >> return CInt)
           <|> try (string "bool" >> return CBool)

typeArraySpec :: CodaParser CTypeArray
typeArraySpec =
    CTypeArray <$> typeSpec <*> subscripts
  where
    subscripts = many $ try $ spaces >> subscript integerLitteral

-- Instructions ----------------------------------------------------------------

compoundStmt :: CodaParser CCompoundStmt
compoundStmt = between (char '{') (char '}')
                       (many stmt)

stmt :: CodaParser CStmt
stmt =     try (CExpr        <$> expr           <* tailingSep)
       <|> try (CDecl        <$> variableDecl   <* tailingSep)
       <|> try (CAssignation <$> assignableExpr <* spaces <* char '=' <* spaces
                             <*> expr           <* tailingSep)
       <|> try (CReturn      <$> (string "return" *> spaces1 *> expr)
                             <*  tailingSep)
       <|> try (CIf          <$> (string "if" *> spaces *> guard) <*  spaces
                             <*> compoundStmt
                             <*> optionMaybe (try $    spaces *> string "else"
                                                    *> spaces *> compoundStmt))
       <|> try (CWhile       <$> (string "while" *> spaces *> guard) <* spaces
                             <*> compoundStmt)
  where
    guard = between (char '(' >> spaces) (spaces >> char ')') expr

-- Expressions -----------------------------------------------------------------

expr, andExpr, comparisonExpr, numericExpr, multiplicativeExpr, valueExpr
    :: CodaParser CExpr
expr = binaryExpr [string "||" >> return COr] andExpr

andExpr = binaryExpr [string "&&" >> return CAnd] comparisonExpr

comparisonExpr = binaryExpr [
      string "==" >> return CEq  , string "!=" >> return CNEq
    , char   '<'  >> return CLt  , char   '>'  >> return CGt
    , string "<=" >> return CLtEq, string ">=" >> return CGtEq
    ] numericExpr

numericExpr = binaryExpr [char '+' >> return CAdd, char '-' >> return CSub]
                         multiplicativeExpr

multiplicativeExpr = binaryExpr [
      char '*' >> return CMult, char '/' >> return CDiv, char '%' >> return CMod
    ] valueExpr

valueExpr =     try (CCall <$> identifier <* spaces <*> callArgs)
            <|> try (CAssignable <$> assignableExpr)
            <|> try (between (char '(') (char ')') (spaces >> expr <* spaces))
            <|> try (CLitteral <$> litteral)
  where
    callArgs = between (char '(' >> spaces) (spaces >> char ')')
                       (expr `sepBy` (spaces >> char ',' >> spaces))

assignableExpr :: CodaParser CAssignableExpr
assignableExpr =
    CAssignableExpr <$> identifier <*> subscripts
  where
    subscripts = many $ subscript expr

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

-- Utilitaires -----------------------------------------------------------------

-- | Parse une fin d'instruction.
tailingSep :: CodaParser Char
tailingSep = spaces >> char ';'

-- | Parse un ensemble d'opérateurs binaires dotés d'une priorité identique.
-- Chaque opérateur est fourni avec le parseur de son symbole. Le second
-- argument parse les opérantes de l'opérateur (càd les l'expression ayant une
-- priorité plus importante). Effectue une association sur la gauche.
binaryExpr :: [CodaParser CBinOp] -> CodaParser CExpr -> CodaParser CExpr
binaryExpr ops inner =
    inner >>= go
  where
    -- Ignore les espaces entre l'opérateur et les opérandes.
    go left = spaces >> tryOperators left ops

    -- Tente de parser chaque symbole des opérateurs et une seconde opérande.
    -- Arrête de parser si aucun symbole ne parse.
    tryOperators left (p:ps) = tryOperator left p <|> tryOperators left ps
    tryOperators left []     = return left

    tryOperator left p = try $ do
        op <- CBinOp <$> p <*> (pure left <* spaces)
                           <*> inner
        go op

-- | Parse une expression de définition ou de déréférencement d'une dimension
-- d'un tableau. Le parseur 'inner' parse le contenu entre [ et ]. Ignore les
-- espaces internes.
subscript :: CodaParser a -> CodaParser a
subscript inner = between (char '[')  (char ']') (spaces *> inner <* spaces)

-- | Parse 1 à N caractères d'espacement.
spaces1 :: CodaParser ()
spaces1 = space >> spaces
