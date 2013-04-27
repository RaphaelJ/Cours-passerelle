-- | Définit un parser Parsec capable de gérérer un 'AST' à partir d'un flux de
-- caractères (Lazy Text).
module Language.Coda.Parser (CodaParser, parser, parse) where

import Control.Applicative ((<$>), (<*>), (<|>), (<*), (*>), pure)
import Control.Monad (when)
import qualified Data.Map as M
import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Parsec (
      SourceName, ParseError, (<?>), alphaNum, between, char, digit, eof
    , getState, letter, many, many1, modifyState, optionMaybe, runParser, sepBy
    , space, spaces, string, try
    )
import Text.Parsec.Text.Lazy (GenParser)

import Language.Coda.AST

-- | Contient l'état du parseur et du vérificateur sémantique.
data ParserState = ParserState {
      psVars :: M.Map CIdent CVar, psFuns :: M.Map CIdent CFun
    }

type CodaParser = GenParser ParserState

parser :: CodaParser AST
parser =
    spaces *> many (declaration <* spaces) <* eof
  where
    declaration =     try (CTopLevelVar <$> variableDecl)
                  <|> try (CTopLevelFun <$> functionDecl)

-- | Exécute le parseur sur un flux de texte.
parse :: SourceName -> TL.Text -> Either ParseError AST
parse = let emptyState = ParserState M.empty M.empty
        in runParser parser emptyState

-- Déclarations ----------------------------------------------------------------

variableDecl :: CodaParser CVarDecl
variableDecl = do
    qual                  <- typeQual <* spaces1

    mType                 <-     try (Just <$> typeArraySpec)
                             <|> (string "auto" >> return Nothing)
    spaces1

    ident                 <- identifier <* spaces
    checkIdentifer ident

    mExpr                 <- optionMaybe (char '=' *> spaces *> expr)

    decl@(CVarDecl var _) <- getVar qual mType ident mExpr
    registerVar var

    tailingSep
    return decl
  where
    -- Vérifie que la variable n'a pas déjà été déclarée.
    checkIdentifer ident = do
        varsSt <- psVars <$> getState
        when (ident `M.member` varsSt) $
            fail $ printf "Variable identifer `%s` is already defined."
                          (T.unpack ident)

    -- Retourne la définition de la variable, en vérifiant que celle-ci est
    -- correctement définie et initialisée.

    -- Variables non initialisées
    getVar CQualConst _                              _     Nothing           =
        fail "A constant must be assigned."
    getVar CQualFree  (Just t)                       ident Nothing           =
        return $ CVarDecl (CVar CQualFree t ident) Nothing
    -- Variables initialisées
    getVar _          (Just (CTypeArray _ (Just _))) _     (Just _)          =
        fail "Arrays can't be assigned."
    getVar qual       (Just t)                       ident (Just (e, tExpr))
        | t == tExpr = return $ CVarDecl (CVar qual t ident) (Just e)
        | otherwise  =
            fail $ printf "Trying to assign an expression of type `%s` to a \
                          \variable of type `%s`." (show tExpr) (show t)
    -- Variables au typage inféré
    getVar qual       Nothing                        ident Nothing           =
        fail $ printf "Can't infer the type of `%s`." (T.unpack ident)
    getVar qual       Nothing                        ident (Just (e, tExpr)) =
        return $ CVarDecl (CVar qual (CTypeArray tExpr Nothing) ident) (Just e)

functionDecl :: CodaParser CFun
functionDecl =
    CFun <$> optionMaybe (typeSpec <* spaces1)
         <*> identifier <* spaces
         <*> args <* spaces
         <*> (    try (Just <$> compoundStmt)
              <|> (char ';' >> return Nothing))
  where
    args = between (char '(' >> spaces) (spaces >> char ')')
                   (arg `sepBy` (spaces >> char ',' >> spaces))

    arg = do
        qual    <- typeQual
        t       <- argType

        mIdent  <- optionMaybe (try $ spaces1 *> identifier)
        case mIdent of
            Just ident -> do
                let var = CVar
                in 
            Nothing    ->
        getArg qual t  mIdent
        CArgument <$> typeArraySpec
                    <*> 
                    <*> 

    argType = do
        t@(CTypeArray prim mDims) <- typeArraySpec
        -- Parse une éventuelle dimension implicite supplémentaire.
            (try $ subscript spaces >>
                   case mDims of
                        Just (nDims, dims) -> 
                            CTypeArray prim (Just (nDims + 1, dims))
                        Nothing            ->
                            CTypeArray prim (Just (1, []))
            )
        <|> return t

    getArg qual t True (Just ident) =
        let var = CVar qual 
        CVarArgument 
    get

    

-- Types -----------------------------------------------------------------------

typeQual :: CodaParser CQual
typeQual =     try (string "const" >> return CQualConst)
           <|> return CQualFree

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
compoundStmt = between (char '{' >> spaces) (char '}') (many (stmt <* spaces))

stmt :: CodaParser CStmt
stmt =     try (CDecl   <$> variableDecl)
       <|> try (CAssign <$> varExpr <* spaces <* char '=' <* spaces
                        <*> expr <* tailingSep)
       <|> try (CExpr   <$> expr <* tailingSep)
       <|> try (CIf     <$> (string "if" *> spaces *> guard) <*  spaces
                        <*> compoundStmt
                        <*> optionMaybe (try $    spaces *> string "else"
                                               *> spaces *> compoundStmt))
       <|> try (CWhile  <$> (string "while" *> spaces *> guard) <* spaces
                        <*> compoundStmt)
  where
    guard = between (char '(' >> spaces) (spaces >> char ')') expr

-- Expressions -----------------------------------------------------------------

-- Parse les opérateurs binaires avec une descente récursive en profondeur sur
-- des opérateurs de plus en plus prioritaire jusqu'à atteindre une expression
-- littérale ou un identifieur.

expr, andExpr, comparisonExpr, numericExpr, multiplicativeExpr, valueExpr
    :: CodaParser (CExpr, CType)
expr = binaryExpr CBool CBool [string "||" >> return COr] andExpr

andExpr = binaryExpr CBool CBool [string "&&" >> return CAnd] comparisonExpr

comparisonExpr = binaryExpr CInt CBool [
      string "==" >> return CEq  , string "!=" >> return CNEq
    , char   '<'  >> return CLt  , char   '>'  >> return CGt
    , string "<=" >> return CLtEq, string ">=" >> return CGtEq
    ] numericExpr

numericExpr = binaryExpr CInt CInt [
      char '+' >> return CAdd, char '-' >> return CSub
    ] multiplicativeExpr

multiplicativeExpr = binaryExpr CInt CInt [
      char '*' >> return CMult, char '/' >> return CDiv, char '%' >> return CMod
    ] valueExpr

valueExpr =     try (CCall     <$> identifier <* spaces <*> callArgs)
            <|> try (CVariable <$> varExpr)
            <|> try (CLitteral <$> litteral)
            <|> try (between (char '(' >> spaces) (spaces >> char ')') expr)
  where
    callArgs = between (char '(' >> spaces) (spaces >> char ')')
                       (expr `sepBy` (spaces >> char ',' >> spaces))

varExpr :: CodaParser CVarExpr
varExpr = do
    var <- identifer >>= getVar
    CVarExpr var <$> getSubs var
  where
    -- Recherche dans la table des symboles la référence à la variable.
    getVar ident = do
        varsSt <- psVars <$> getState
        case ident `M.lookup` varsSt of
            Just var -> return var
            Nothing  ->
                fail $ printf "Unknown identifer : `%s`." (T.unpack ident)

    getSubs (CVar _ (CTypeArray _ dims) _) = do
        subs <- many $ subscript subs
        case (dims, subs) of
            -- Variable scalaire
            (Nothing, []   ) -> return []
            (Nothing, (_:_)) ->
                fail $ printf "Trying to subscript `%s` which is a scalar."
                              (T.unpack ident)
            -- Tableau
            (Just ds, ss   ) -> 
                let nDims = length ds + 1
                    goSub ds ss

    goSub [] [(s, tExpr)] = [s]
    goSub [] [] = fail
    

litteral :: CodaParser CLitteral
litteral =     (CLitteralInt  <$> integerLitteral)
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
    -- Parse l'expression de gauche "jusqu'au plus loin possible".
    inner >>= go
  where
    -- Ignore les espaces entre la première opérande et l'opérateur.
    go left = spaces >> tryOperators left ops

    -- Tente de parser chaque symbole des opérateurs et une seconde opérande.
    -- Arrête de parser si aucun symbole ne parse.
    tryOperators left (p:ps) = tryOperator left p <|> tryOperators left ps
    tryOperators left []     = return left

    tryOperator left p = try $ do
        op <- CBinOp <$> p <*> pure left <* spaces
                           <*> inner
        go op -- Parce une occurrence suivante de ces mêmes opérateurs.

-- | Parse une expression de définition ou de déréférencement d'une dimension
-- d'un tableau. Le parseur \'inner\' parse le contenu entre [ et ]. Ignore les
-- espaces internes entourant ce parseur.
subscript :: CodaParser a -> CodaParser a
subscript inner = between (char '[' >> spaces)  (spaces >> char ']') inner

-- | Parse 1 à N caractères d'espacement.
spaces1 :: CodaParser ()
spaces1 = space >> spaces

-- | Enregistre la variable dans la table des symboles.
registerVar :: CVar -> CodaParser ()
registerVar var@(CVar _ _ ident _) =
    modifyState $ \state ->
        state { psVars = M.insert ident var (psVars state) }
