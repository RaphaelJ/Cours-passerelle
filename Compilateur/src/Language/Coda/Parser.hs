{-# LANGUAGE PatternGuards, TupleSections#-}
-- | Définit un parser Parsec capable de gérérer un 'AST' à partir d\'un flux de
-- caractères (Lazy Text). Le parseur vérifie la cohérence de l\'arbre
-- syntaxique simultanément avec le parsage.
module Language.Coda.Parser (CParser, parse, parser) where

import Control.Arrow ((***))
import Control.Applicative ((<$>), (<*>), (<|>), (<*), (*>))
import Control.Monad (when)
import qualified Data.Map as M
import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Parsec (
      SourceName, ParseError, (<?>), alphaNum, between, char, digit, eof
    , getState, letter, many, many1, modifyState, optionMaybe, putState
    , runParser, sepBy, space, spaces, string, try
    )
import Text.Parsec.Text.Lazy (GenParser)

import Language.Coda.AST

-- | Contient l'état du parseur et du vérificateur sémantique.
data ParserState = ParserState {
      psVars :: M.Map CIdent CVar, psFuns :: M.Map CIdent CFun
    }

type CParser = GenParser ParserState

-- | Exécute le parseur sur un flux de texte.
parse :: SourceName -> TL.Text -> Either ParseError AST
parse = let emptyState = ParserState M.empty M.empty
        in runParser parser emptyState

parser :: CParser AST
parser = do
    spaces >> many (functionDecl <* spaces) >> eof
    (AST . M.elems . psFuns) <$> getState

-- Déclarations ----------------------------------------------------------------

functionDecl :: CParser CFun
functionDecl = do
    tRet  <- optionMaybe $ try typeSpec <* spaces1
    ident <- identifier <* spaces
    tArgs <- args       <* spaces

    -- Enregistre la déclaration de la fonction avant la définition pour
    -- permettre les appels récursifs.
    let decl = CFun tRet ident tArgs Nothing
    registerFun decl

    (    (do -- Sauvegarde l'état des tables de symboles avant la déclaration 
             -- des arguments.
             st <- getState
             mapM_ registerArg (caArgs tArgs)

             stmts@(CCompoundStmt _ hasRet) <- compoundStmt tRet
             putState st
             case (tRet, hasRet) of
                 (Just _, False) ->
                    fail $ printf "Function `%s` doesn't always return a value"
                                  (show ident)
                 _ -> do
                    let def = CFun tRet ident tArgs (Just stmts)
                    registerFun def -- Rajoute la définition.
                    return def)
     <|> (char ';' >> return decl))
  where
    args = CArgs <$> between (char '(' >> spaces) (char ')')
                             ((arg <* spaces) `sepBy` (char ',' >> spaces))

    -- Parse un argument et l'enregistre dans la table de symboles s'il est
    -- associé à une variable.
    arg = do
        t       <- argType
        mIdent  <- optionMaybe $ try $ spaces1 *> identifier
        case mIdent of Just ident -> do checkVarIdent ident
                                        return $ CVarArg (CVar t ident)
                       Nothing    -> return $ CAnonArg t

    registerFun fun@(CFun tRet ident fArgs mStmts) = do
        st <- psFuns <$> getState
        case ident `M.lookup` st of
            Just (CFun tRet' _ fArgs' mStmts') | tRet' /= tRet ->
                fail $ printf "Function `%s` is already declared with a \
                              \different return type" (show ident)
                                               | fArgs /= fArgs' ->
                fail $ printf "Function `%s` is already declared with different\
                              \ argument types" (show ident)
                                              | otherwise     ->
 
                -- Ajoute la définition. Accepte plusieurs déclarations mais une
                -- seule définition.
                case (mStmts, mStmts') of
                    (Just _ , Just _)  ->
                        fail $ printf "Multiple definitions of function `%s`"
                                      (show ident)
                    (Just _ , Nothing) -> registerFun' fun
                    _                  -> return ()
            Nothing -> registerFun' fun

    registerFun' fun@(CFun _ ident _ _) =
        modifyState $ \st -> st { psFuns = M.insert ident fun (psFuns st) }

    registerArg (CVarArg var) = registerVar var
    registerArg (CAnonArg _)  = return ()

variableDecl :: CParser CVarDecl
variableDecl = do
    eType                   <- varType    <* spaces1
    ident                   <- identifier <* spaces
    checkVarIdent ident

    mExpr                   <- optionMaybe rightAssignExpr

    decl@(CVarDecl var _ _) <- getVar eType ident mExpr
    registerVar var

    tailingSep
    return decl
  where
    -- Retourne la définition de la variable, en vérifiant que celle-ci est
    -- correctement définie et initialisée.

    -- Variables non initialisées.
    getVar (Left (CTypeArray CQualConst _ _, _))      _     Nothing          =
        fail "A constant must be assigned"
    getVar (Right _)                                 ident Nothing           =
        fail $ printf "Can't infer the type of `%s`" (show ident)

    -- Variables au type déclaré explicitement.
    getVar (Left (t, n))                             ident Nothing           =
        return $ CVarDecl (CVar t ident) n Nothing
    getVar (Left (CTypeArray _ _ (CArray _ _), _))   _     (Just _)          =
        fail "Arrays can't be assigned"
    getVar (Left (t@(CTypeArray _ prim CScalar), _)) ident (Just (e, prim'))
        | prim /= prim' =
            fail $ printf "Trying to assign an expression of type `%s` to a \
                          \declaration of type `%s`" (show prim') (show prim)
        | otherwise     = return $ CVarDecl (CVar t ident) 1 (Just e)

    -- Variables au type inféré.
    getVar (Right qual)                               ident (Just (e, prim')) =
        let t = CTypeArray qual prim' CScalar
        in return $ CVarDecl (CVar t ident) 1 (Just e)

-- Types -----------------------------------------------------------------------

typeQual :: CParser CQual
typeQual =     (string "const" >> spaces1 >> return CQualConst)
           <|> return CQualFree

typeSpec :: CParser CType
typeSpec =     (string "int"  >> return CInt)
           <|> (string "bool" >> return CBool)

-- | Parse un type explicite avec la taille de ses dimensions ou @auto@ avec son
-- qualifieur.
varType :: CParser (Either (CTypeArray, CInt) CQual)
varType = do
    qual <- typeQual
    (    (do t         <- typeSpec
             (dims, n) <- typeDims
             return $ Left (CTypeArray qual t dims, n))
     <|> (string "auto" >> return (Right qual)))

-- | Parse le type de l'argument avec la première dimension éventuellement
-- implicite (sans taille).
argType :: CParser CTypeArray
argType = do
    t         <- CTypeArray <$> typeQual <*> typeSpec
    mImplicit <- optionMaybe $ try $ subscript spaces
    (dims, n) <- typeDims

    -- Rajoute l'éventuelle première dimension implicite.
    let dims' | Just _ <- mImplicit =
                    case dims of
                        CScalar        -> CArray 1       []
                        CArray d sizes -> CArray (d + 1) (n : sizes)
              | otherwise           = dims
    return $ t dims'

-- | Parse les définitions des tailles des dimensions d\'un tableau.
-- Retourne les dimensions de la variable et le nombre d\'éléments.
typeDims :: CParser (CDims, CInt)
typeDims = do
    sizes <- many $ subscript integerLitteral
    let dims = case sizes of []     -> CScalar
                             (_:ss) -> CArray (length sizes) (paddings ss)
    return (dims, product sizes)

-- | Pré-calcule les tailles des dimensions des tableaux pour le calcul des
-- indices à partir des tailles individuelles de chaque dimension.
paddings :: [CInt] -> [CInt]
paddings = init . scanr (*) 1

-- Instructions ----------------------------------------------------------------

-- | Parse un ensemble d\'instructions. Le type donné en argument donne le
-- type de retour de la fonction. Retourne 'True' si le bloc retourne toujours
-- une valeur quelque soit le chemin d\'exécution.
compoundStmt :: Maybe CType -> CParser CCompoundStmt
compoundStmt retType = do
    st <- getState -- Sauvegarde l'état des tables de symboles.
    char '{' *> spaces *> goCompound False <* char '}' <* putState st
  where
    -- Parse les instructions et vérifie qu'aucune instruction du bloc n'est
    -- inaccessible.
    goCompound precRet =
            (do (x, ret) <- stmt retType <* spaces
                -- Erreur si l'instruction précédente retournait une valeur
                -- car on a pu parser une nouvelle instruction.
                if precRet then fail "Unreachable statement"
                           else do stmts <- goCompound ret
                                   return stmts { csStmts = x : csStmts stmts })
        <|> return (CCompoundStmt [] precRet)

-- | Parse une instruction. Le type donné en argument donne le type de retour de
-- la fonction. Retourne 'True' si l'instruction retourne toujours une valeur
-- quelque soit le chemin d\'exécution.
stmt :: Maybe CType -> CParser (CStmt, Bool)
stmt retType =
        try returnStmt
    <|> try ifStmt
    <|> try ((, False) <$> (CWhile <$> (string "while" *> spaces *> guard)
                                   <*  spaces
                                   <*> compoundStmt retType))
    <|> try assign
    <|> try ((, False) <$> (CDecl  <$> variableDecl))
    <|> try ((, False) <$> (CExpr  <$> (fst <$> expr)
                                   <* tailingSep))
  where
    returnStmt = do
        _ <- string "return"
        ret <- case retType of
            Just prim  -> do
                (e, tExpr) <- spaces1 >> expr
                case tExpr of
                    Nothing                                           ->
                        fail $ printf "Must return a value of type `%s`" 
                                      (show prim)
                    Just (CTypeArray _ _     (CArray _ _))            ->
                        fail "Function can't return array expressions"
                    Just (CTypeArray _ prim' CScalar) | prim /= prim' ->
                        fail $ printf "Return's expression type (`%s`) doesn't\
                                      \ match the function type (`%s`)"
                                      (show prim') (show prim)
                                                      | otherwise     ->
                        return $ CReturn (Just (e, prim))

            Nothing -> return $ CReturn Nothing
        spaces >> tailingSep >> return (ret, True)

    ifStmt = do
        cond       <- (string "if" *> spaces *> guard) <* spaces

        ifStmts <- compoundStmt retType <* spaces
        mElse   <- optionMaybe (string "else" *> spaces *> compoundStmt retType)

        -- Le bloc retourne si le bloc du if et le bloc du else retournent.
        let (elseStmts, ret) = case mElse of
                Just stmts -> (Just stmts, csReturn ifStmts && csReturn stmts)
                Nothing    -> (Nothing, False)
        return (CIf cond ifStmts elseStmts, ret)

    assign = do
        (eLeft, tLeft)      <- varExpr <* spaces
        (eRight, primRight) <- rightAssignExpr <* tailingSep

        case tLeft of
            CTypeArray CQualConst _ _   -> 
                fail "Can't assign a constant"
            CTypeArray _ _ (CArray _ _) ->
                fail "Left expression of an assignation must be a scalar"
            CTypeArray _ primLeft CScalar | primLeft /= primRight ->
                fail $ printf "Trying to assign an expression of type `%s` to a\
                              \ variable of type `%s`"
                              (show primRight) (show primLeft)
                                          | otherwise ->
                return (CAssign eLeft eRight, False)

    guard = do
        (e, tExpr) <- between (char '(' >> spaces) (spaces >> char ')') expr
        case tExpr of
            Just (CTypeArray _ CBool CScalar) -> return e
            _ -> fail "Guard must be a boolean expression"

-- | Parse le signe d\'assignation (=) et l\'expression qui se trouve à sa
-- droite. Vérifie que l\'expression retourne un scalaire.
rightAssignExpr :: CParser (CExpr, CType)
rightAssignExpr = do
    (e, tExpr) <- char '=' *> spaces *> expr
    case tExpr of
        Nothing -> fail "Trying to assign a void expression to a variable"
        Just (CTypeArray _ _    (CArray _ _)) ->
            fail "Trying to assign an array expression to a variable"
        Just (CTypeArray _ prim CScalar) -> return (e, prim)

-- Expressions -----------------------------------------------------------------

-- Parse les opérateurs binaires avec une descente récursive en profondeur sur
-- des opérateurs de plus en plus prioritaire jusqu'à atteindre une expression
-- littérale ou un identifiant.

expr, andExpr, comparisonExpr, numericExpr, multiplicativeExpr, valueExpr
    :: CParser (CExpr, Maybe CTypeArray)
expr = binaryExpr CBool CBool [COr] andExpr

andExpr = binaryExpr CBool CBool [CAnd] comparisonExpr

comparisonExpr = binaryExpr CInt CBool [CEq, CNEq, CLtEq, CGtEq, CLt, CGt]
                            numericExpr

numericExpr = binaryExpr CInt CInt [CAdd, CSub] multiplicativeExpr

multiplicativeExpr = binaryExpr CInt CInt [CMult, CDiv, CMod] valueExpr

valueExpr =     callExpr
            <|> ((CLitteral *** (Just . toTypeArray)) <$> try litteral)
            <|> ((CVariable *** Just)                 <$> varExpr)
            <|> (between (char '(' >> spaces) (spaces >> char ')') expr)
  where
    callExpr = do
        (fun@(CFun ret _ _ _), args) <- call
        return (CCall fun args, toTypeArray <$> ret)

    toTypeArray ctype = CTypeArray CQualConst ctype CScalar

-- | Parse un appel de fonction.
call :: CParser (CFun, [CExpr])
call = do
    (ident, args) <- try $
        (,) <$> identifier <* spaces
            <*> between (char '(' >> spaces) (spaces >> char ')')
                        (expr `sepBy` (spaces >> char ',' >> spaces))

    fun@(CFun _ _ (CArgs fArgs) _) <- getFun ident
    let n  = length args
        n' = length fArgs
    if n /= n' then fail $ printf "Trying to apply %d argument(s) to a function\
                                  \ of %d argument(s)" n n'
               else (fun,) <$> argsExprs ident 1 fArgs args
  where
    -- Recherche dans la table des symboles la référence à la fonction.
    getFun ident = do
        funsSt <- psFuns <$> getState
        case ident `M.lookup` funsSt of
            Just fun -> return fun
            Nothing  -> fail $ printf "Unknown function identifer : `%s`" 
                                      (show ident)

    -- Vérifie le type de chaque expression assignée aux arguments de la
    -- fonction.
    argsExprs ident i _            ((_, Nothing):_)             =
         fail $ printf "Given a void expression as argument #%d of the call to \
                       \the function `%s`" i (show ident)
    argsExprs ident i (fArg:fArgs) ((eArg, Just tArg):args)
        | tFunArg <- getArgType fArg, not $ compatibleTypes tArg tFunArg =
            fail $ printf "Argument #%d of the call to the function `%s` is of \
                          \type `%s` whereas the given expression is of type \
                          \`%s`"  i (show ident) (show tFunArg) (show tArg)
        | otherwise = (eArg:) <$> argsExprs ident (i + 1 :: Int) fArgs args
    argsExprs _     _ ~[]          ~[]                          = return []

    compatibleTypes (CTypeArray _ prim dims) (CTypeArray _ prim' dims')
        = prim == prim' && compatibleDims dims dims'

    compatibleDims CScalar      CScalar      = True
    compatibleDims (CArray _ _) (CArray _ _) = True
    compatibleDims _            _            = False

varExpr :: CParser (CVarExpr, CTypeArray)
varExpr = do
    var@(CVar (CTypeArray q prim dims) _) <- identifier >>= getVar
    (subs, dims')                         <- subsExprs dims
    return (CVarExpr var dims' subs, CTypeArray q prim dims')
  where
    -- Recherche dans la table des symboles la référence à la variable.
    getVar ident = do
        varsSt <- psVars <$> getState
        case ident `M.lookup` varsSt of
            Just var -> return var
            Nothing  -> fail $ printf "Unknown variable identifier : `%s`"
                                      (show ident)

    -- Déférence éventuellement les dimensions d'un tableau.
    subsExprs dims = do
            (do sub <- subsExpr
                (subs, dims') <- case dims of
                    CScalar     -> fail "Trying to subscript a scalar variable"
                    CArray 1 []      -> subsExprs CScalar
                    CArray n ~(_:ss) -> subsExprs (CArray (n - 1) ss)
                return (sub:subs, dims'))
        <|> return ([], dims)

    subsExpr = do
        (sub, tSub) <- subscript expr
        case tSub of
            Just (CTypeArray _ CInt CScalar) -> return sub
            _ -> fail "Subscripts must be scalar integer expressions"

litteral :: CParser (CLitteral, CType)
litteral =     (((, CInt)  . CLitteralInt)  <$> integerLitteral)
           <|> (((, CBool) . CLitteralBool) <$> boolLitteral)

integerLitteral :: CParser CInt
integerLitteral = read <$> many1 digit
                       <?> "integer litteral"

boolLitteral :: CParser CBool
boolLitteral =     (string "true"  >> return True)
               <|> (string "false" >> return False)

identifier :: CParser CIdent
identifier =     ((CIdent . T.pack) <$> ((:) <$> letter
                                             <*> many (alphaNum <|> char '_')))
             <?> "identifier"

-- Utilitaires -----------------------------------------------------------------

-- | Parse un ensemble d\'opérateurs binaires dont la priorité, le type des
-- opérandes et le type de retour sont identiques.
-- Le premier et le second argument donnent respectivement le type des
-- opérantes et le type de retour de l\'application des opérateurs.
-- Chaque opérateur est fourni avec le parseur de son symbole. Le dernier
-- argument parse les opérantes de l\'opérateur (càd les l\'expression ayant une
-- priorité plus importante). Effectue une association sur la gauche.
binaryExpr :: CType -> CType -> [CBinOp] -> CParser (CExpr, Maybe CTypeArray)
           -> CParser (CExpr, Maybe CTypeArray)
binaryExpr tInner tRet ops inner =
    -- Parse l'expression de gauche "jusqu'au plus loin possible".
    inner >>= go
  where
    -- Ignore les espaces entre la première opérande et l'opérateur.
    go left = spaces >> tryOperators left ops

    -- Tente de parser chaque symbole des opérateurs et une seconde opérande.
    -- Arrête de parser si aucun symbole ne parse.
    tryOperators left (op:ops') = tryOperator left op <|> tryOperators left ops'
    tryOperators left []        = return left

    tryOperator (eLeft, tLeft) op = do
        _ <- try $ string $ show op
        checkInnerType op "left" tLeft
        (eRight, tRight) <- spaces *> inner
        checkInnerType op "right" tRight

        -- Parse une occurrence suivante de ces mêmes opérateurs.
        go (CBinOp op eLeft eRight, Just (CTypeArray CQualConst tRet CScalar))

    checkInnerType op side Nothing                              =
        fail $ printf "Trying to apply a void expression to the %s argument of \
                      \a `%s` operator" side (show op)
    checkInnerType op side (Just (CTypeArray _ _ (CArray _ _))) =
        fail $ printf "Trying to apply an array expression to the %s argument \
                      \of a `%s` operator" side (show op)
    checkInnerType op side (Just (CTypeArray _ prim CScalar)) | prim /= tInner =
        fail $ printf "Trying to apply a `%s` expression to the %s argument of \
                      \a `%s` operator" (show prim) side (show op)
                                                              | otherwise      =
        return ()

-- | Vérifie que la variable n\'a pas déjà été déclarée.
checkVarIdent :: CIdent -> CParser ()
checkVarIdent ident = do
    varsSt <- psVars <$> getState
    when (ident `M.member` varsSt) $
        fail $ printf "Variable identifer `%s` is already defined"
                      (show ident)

-- | Enregistre la variable dans la table des symboles.
registerVar :: CVar -> CParser ()
registerVar var@(CVar _ ident) =
    modifyState $ \st -> st { psVars = M.insert ident var (psVars st) }

-- | Parse une expression de définition ou de déréférencement d\'une dimension
-- d\'un tableau. Le parseur @inner@ parse le contenu entre [ et ]. Ignore les
-- espaces de début et ceux entourant ce parseur.
subscript :: CParser a -> CParser a
subscript inner =
    try (spaces >> char '[') *> spaces *> inner <* spaces <* char ']'

-- | Parse 1 à N caractères d\'espacement.
spaces1 :: CParser ()
spaces1 = space >> spaces

-- | Parse jusqu\'à la fin d'instruction.
tailingSep :: CParser ()
tailingSep = spaces >> char ';' >> return ()
