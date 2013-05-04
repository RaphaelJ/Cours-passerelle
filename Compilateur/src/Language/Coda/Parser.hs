{-# LANGUAGE TupleSections #-}
-- | Définit un parser Parsec capable de gérérer un 'AST' à partir d'un flux de
-- caractères (Lazy Text).
module Language.Coda.Parser (CodaParser, parser, parse) where

import Control.Arrow (first)
import Control.Applicative ((<$>), (<*>), (<|>), (<*), (*>), empty, pure)
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
                  <|>     (CTopLevelFun <$> functionDecl)

-- | Exécute le parseur sur un flux de texte.
parse :: SourceName -> TL.Text -> Either ParseError AST
parse = let emptyState = ParserState M.empty M.empty
        in runParser parser emptyState

-- Déclarations ----------------------------------------------------------------

variableDecl :: CodaParser CVarDecl
variableDecl = do
    eType                 <- typeArraySpecInfer <* spaces1
    ident                 <- identifier <* spaces
    checkVarIdent ident

    mExpr                 <- optionMaybe (char '=' *> spaces *> expr)

    decl@(CVarDecl var _) <- getVar eType ident mExpr
    registerVar var

    tailingSep
    return decl
  where
    -- Vérifie que la variable n'a pas déjà été déclarée.
    checkVarIdent ident = do
        varsSt <- psVars <$> getState
        when (ident `M.member` varsSt) $
            fail $ printf "Variable identifer `%s` is already defined."
                          (T.unpack ident)

    -- Retourne la définition de la variable, en vérifiant que celle-ci est
    -- correctement définie et initialisée.

    -- Variables non initialisées.
    getVar (Left (CTypeArray CQualConst _ _))   _     Nothing           =
        fail "A constant must be assigned."
    getVar (Right _)                            ident Nothing           =
        fail $ printf "Can't infer the type of `%s`." (T.unpack ident)
    -- Variables au type déclaré explicitement.
    getVar (Left t)                             ident Nothing           =
        return $ CVarDecl (CVar t ident) Nothing
    getVar (Left t@(CTypeArray _ _ (Just _)))   ident (Just _)          =
        fail "Arrays can't be assigned."
    getVar (Left t@(CTypeArray _ prim Nothing)) ident (Just (e, tExpr))
        | prim == tExpr = return $ CVarDecl (CVar t ident) (Just e)
        | otherwise     =
            fail $ printf "Trying to assign an expression of type `%s` to a \
                          \declaration of type `%s`." (show tExpr) (show prim)
    -- Variables au type inféré.
    getVar (Right qual )                        ident (Just (e, tExpr)) =
        return $ CVarDecl (CVar (CTypeArray qual tExpr Nothing) ident) (Just e)

functionDecl :: CodaParser CFun
functionDecl =
    tRet  <- optionMaybe (try $ typeSpec <* spaces1)
    ident <- identifer <* spaces
    tArgs <- args <* spaces

    -- Enregistre la déclaration de la fonction avant la définition pour
    -- permettre les appels récursifs.
    let decl = CFun tRet ident tArgs Nothing
    registerFun decl

    (    (do def <- CFun tRet ident tArgs <$> compoundStmt
             registerFun def
             return def)
     <|> (char ';' >> return decl))
  where
    args = between (char '(' >> spaces) (spaces >> char ')')
                   (CArguments <$> (arg `sepBy` (spaces >> char ',' >> spaces)))

    -- Parse un argument et l'enregistre dans la table de symboles s'il est
    -- associé à un identifier.
    arg = do
        t       <- argType

        mIdent  <- optionMaybe (try $ spaces1 *> identifier)
        case mIdent of Just ident -> do let var = CVar t ident
                                        registerVar var
                                        return $ CVarArgument var
                       Nothing    -> return $ CAnonArgument t

    -- Parse le type de l'argument et une éventuelle dimension implicite
    -- supplémentaire.
    argType = do
        t@(CTypeArray qual prim mDims) <- typeArraySpec
        (    (subscript spaces >>
              case mDims of
                Just (n, dims) ->
                    return $ CTypeArray qual prim (Just (n + 1, dims))
                Nothing            ->
                    return $ CTypeArray qual prim (Just (1, [])))
         <|> return t)

    registerFun fun@(CFun tRet ident args mStmts) =
        st <- psFuns <$> getState
        case ident `M.lookup` st of
            Just (CFun tRet' _ args' mStmts') | tRet' /= tRet ->
                fail $ printf "Function `%s` is already declared with a \
                              \different return type." ident
                                              | args /= args' ->
                fail $ printf "Function `%s` is already declared with \
                              \different argument types." ident
                                              | otherwise     ->
                case (mStmts, mStmts') of
                    (Just _ , Just _)  ->
                        fail $ printf "Multiple definitions of function `%s`."
                                      ident
                    (Just _ , _)       ->
                        modifyState $ \st ->
                            st { psFuns = M.insert ident fun (psFuns st) }
            Nothing ->
                modifyState $ \st ->
                    st { psFuns = M.insert ident fun (psFuns st) }

-- Types -----------------------------------------------------------------------

typeQual :: CodaParser CQual
typeQual =     (string "const" >> return CQualConst)
           <|> return CQualFree

typeSpec :: CodaParser CType
typeSpec =     (string "int"  >> return CInt)
           <|> (string "bool" >> return CBool)

-- | Parse un type explicite ou @auto@ avec son qualifieur.
varType :: CodaParser (Either CTypeArray CQual)
varType = do
    qual <- typeQual <* spaces1
    (    (Left <$> CTypeArray qual <$> typeSpec <*> typeSubscripts)
     <|> (string "auto" >> return (Right qual)))

typeArgSpec :: CodaParser CTypeArray
typeArgSpec 

typeSubscripts :: CodaParser (CDims, [Int])
typeSubscripts = do
    sizes <- many $ subscript integerLitteral <* spaces
    let dims = case sizes of []     -> CScalar
                             (_:ss) -> CArray (length sizes) ss
    return (dims, sizes)

-- Instructions ----------------------------------------------------------------

-- | Parse un ensemble d\'instructions. Le type donné en argument donne le
-- type de retour de la fonction. Retourne 'True' si le bloc retourne toujours
-- une valeur quelque soit le chemin d\'exécution.
compoundStmt :: Maybe CType -> CodaParser (CCompoundStmt, Bool)
compoundStmt retType = do
    st <- getState -- Sauvegarde l'état des tables de symboles.
    char '{' *> spaces *> goCompound Nothing <* char '{' <* putState st
  where
    -- Parse les instructions et vérifie qu'aucune instruction du bloc n'est
    -- inaccessible.
    goCompound precRet =
            (do (x, ret) <- stmt retType <* spaces
                -- Erreur si l'instruction précédente retournait une valeur
                -- car on a pu parser une nouvelle instruction.
                -- Sinon, on continue.
                if precRet then fail "Unreachable statement."
                           else first (x :) <$> goCompound ret)
        <|> return ([], precRet)

-- | Parse une instruction. Le type donné en argument donne le type de retour de
-- la fonction. Retourne 'True' si l'instruction retourne toujours une valeur
-- quelque soit le chemin d\'exécution.
stmt :: Maybe CType -> CodaParser (CStmt, Bool)
stmt retType =
        try returnStmt
    <|> try ifStmt
    <|> try ((, False) <$> CWhile  <$> (string "while" *> spaces *> guard)
                                   <*  spaces
                                   <*> compoundStmt)
    <|> try assign
    <|> try ((, False) <$> CDecl   <$> variableDecl)
    <|> try ((, False) <$> CExpr   <$> expr <* tailingSep)
  where
    returnStmt = do
        string "return"
        stmt <- case retType of
            Just t  -> do
                (e, tExpr) <- spaces1 >> expr
                if t == tExpr
                    then fail $ printf "Return's expression type (`%s`) doesn't\
                                       \ match the function type (`%s`)."
                                       (show tExpr) (show t)
                    else return $ CReturn (Just e)
            Nothing -> return $ CReturn Nothing
        spaces >> tailingSep >> return (stmt, True)

    ifStmt = do
        cond             <- (string "if" *> spaces *> guard) <* spaces

        (ifStmts, ifRet) <- compoundStmt retType <* spaces
        mElse            <- optionMaybe (string "else" *> spaces *>
                                         compoundStmt retType)

        let (elseStmts, ret) = case mElse of
                Just (elseStmts, elseRet) -> (Just elseStmts, ifRet && elseRet)
                Nothing                   -> (Nothing       , ifRet)
        return (CIf cond ifStmts elseStmts, ret)

    assign = do
        (eLeft, (qLeft, tLeft)) <- varExpr <* spaces <* char '=' <* spaces
        (eRight, tRight)        <- expr    <* tailingSep

        case (qLeft, tLeft, tRight) of
            (CQualConst, _   , _)     -> fail "Can't assign a constant."
            (CQualFree , left, right) | left /= right ->
                fail $ printf "Trying to assign an expression of type `%s` to a\
                              \ variable of type `%s`." (show right) (show left)
                                      | otherwise     ->
                return (CAssign eLeft eRight, False)

    guard = do
        (e, tExpr) <- between (char '(' >> spaces) (spaces >> char ')') expr
        if tExpr == CBool then return e
                          else fail "Guard must be a boolean expression."

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

varExpr :: CodaParser (CVarExpr, (CQual, CType))
varExpr = do
    var <- identifier >>= getVar
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
        subs <- many $ try $ spaces >> subscript expr
        case (dims, subs) of
            -- Variable scalaire
            (Nothing, []   ) -> return []
            (Nothing, (_:_)) ->
                fail $ printf "Trying to subscript a scalar variable."
            -- Tableau
            (Just (n, _), ss)
                | n' <- length ss, n' /= n ->
                    fail $ printf "Dereferencing `%d` dimension(s) of a `%d` \
                                  \dimension(s) array." n' n
                | any ((/= CInt) . snd) ss ->
                    fail $ printf "Subscripts must be integer expressions."
                | otherwise -> return $ map fst ss

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

-- | Parse jusqu\'à la fin d'instruction.
tailingSep :: CodaParser Char
tailingSep = spaces >> char ';'

-- | Parse un ensemble d'opérateurs binaires dotés d'une priorité identique.
-- Le premier et le second argument donnent respectivement le type des
-- opérantes et le type de retour de l'application des opérateurs.
-- Chaque opérateur est fourni avec le parseur de son symbole. Le dernier
-- argument parse les opérantes de l'opérateur (càd les l'expression ayant une
-- priorité plus importante). Effectue une association sur la gauche.
binaryExpr :: CType -> CType -> [CodaParser CBinOp] -> CodaParser (CExpr, CType)
           -> (CodaParser CExpr, CType)
binaryExpr tInner tRet ops inner =
    -- Parse l'expression de gauche "jusqu'au plus loin possible".
    inner >>= go
  where
    -- Ignore les espaces entre la première opérande et l'opérateur.
    go left = spaces >> tryOperators left ops

    -- Tente de parser chaque symbole des opérateurs et une seconde opérande.
    -- Arrête de parser si aucun symbole ne parse.
    tryOperators left (p:ps) = tryOperator left p <|> tryOperators left ps
    tryOperators left []     = return left

    tryOperator (eLeft, tLeft) p = do
        op               <- p
        when (tLeft /= tInner) $ opTypeError tLeft "left"
        (eRight, tRight) <- spaces *> inner
        when (tRight /= tInner) $ opTypeError tRight "right"

        -- Parse une occurrence suivante de ces mêmes opérateurs.
        go (CBinOp op eLeft eRight)

    opTypeError tOp side =
        fail $ printf "Trying to a apply an expression of type `%s` to the %s \
                      \argument of an `%s` operator."
                      (show tOp) side (show tInner)

-- | Parse une expression de définition ou de déréférencement d'une dimension
-- d'un tableau. Le parseur @inner@ parse le contenu entre [ et ]. Ignore les
-- espaces internes entourant ce parseur.
subscript :: CodaParser a -> CodaParser a
subscript inner = between (char '[' >> spaces)  (spaces >> char ']') inner

-- | Parse 1 à N caractères d'espacement.
spaces1 :: CodaParser ()
spaces1 = space >> spaces

-- | Enregistre la variable dans la table des symboles.
registerVar :: CVar -> CodaParser ()
registerVar var@(CVar _ _ ident _) =
    modifyState $ \st ->
        st { psVars = M.insert ident var (psVars st) }
