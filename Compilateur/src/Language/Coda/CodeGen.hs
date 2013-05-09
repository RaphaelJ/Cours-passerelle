{-# LANGUAGE OverloadedStrings #-}
-- | Définit le générateur de code qui génère le code IR d\'LLVM à partir de
-- l\'arbre syntaxique du programme.
module Language.Coda.CCodeGen (genCode, codeGen) where

import Control.Monad (mapM, mapM_)
import Control.Monad.State (State, get, put, runState, state)
import Data.Monoid ((<>), mconcat)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, fromString, fromText, toLazyText)

import Language.Code.AST

-- | Monade utilisée lors de la génération des fonctions pour émettre du code
-- IR, pour garder le comptage des identifiants et pour retenir la
-- correspondance entre les identifiants de l\''AST' et ceux de LLVM.
type CCodeGen = WriterT TL.Builder (State CCodeGenState)

data CCodeGenState = CCodeGenState {
      cgsNextIdent :: !Int, cgsVarPtrs :: M.Map CIdent LLVMVPtr
    }

genCode :: AST -> TL.Text
genCode = toLazyText . mconcat . map cFun . astFuns

-- Fonctions -------------------------------------------------------------------

cFun :: CFun -> Builder
cFun (CFun ret ident args mStmts) = do
    let ret' = case ret of Just t -> cType t
                           _      -> "void"

    let fType = emit ret' <-> emit ident
    return $ case mStmts of
        Just stmts ->
            let (args', instrs) = runCodeGen $ do
                    stacked <- mapM stackArg args
                    mapM_ snd stacked -- Arguments scalaires sur la pile
                    cStmts stmts      -- Génère la fonction
                    return $ map fst stacked
            in "define "  <> fType <> "(" <> (mconcat (intercalate ", " args'))
                                   <> ") { \n" <> instrs <> "}"

        Nothing    ->
            let args' = intercalate ", " $ map (emit . getArgType) args
            in "declare " <> fType <> "(" <> mconcat args' <> ")"
  where
    runCodeGen = flip runState emptyState . snd . runWriterT

    -- Alloue un identifiant pour l'argument et retourne sa déclaration ainsi
    -- qu'une action qui sera exécutée en début de génération de la fonction
    -- (pour copier la valeur des arguments scalaires sur la pile).
    stackArg :: CArg -> CCodeGen (Builder, CCodeGen ())
    stackArg (CVarArg var@(CVar t@(CTypeArray _ prim dims) varIdent)) = do
        i <- newIdent

        -- Copie les arguments scalaires sur la pile mais référence directement
        -- les tableaux par leur pointeur (passage par référence).
        let action  = case dims of
                CScalar    -> do
                    ptr <- cVarDecl $ CVarDecl var 1 Nothing
                    store ptr i
                CArray _ _ -> registerVar varIdent i

        return (emit t <-> emit i, action)
    stackArg (CAnonArg t) = return (emit t, return ())

    emptyState = CCodeGenState 1 M.empty

-- Types -----------------------------------------------------------------------

class Emittable a where
    emit :: a -> Builder

instance Emittable CType where
    emit CInt  = "i64"
    emit CBool = "i1"

instance Emittable (Maybe CType) where
    emit (Just t) = emit t
    emit Nothing  = "void"

instance Emittable TypeArray where
    emit (CTypeArray _ prim CScalar)      = emit prim
    emit (CTypeArray _ prim (CArray _ _)) = emit prim <> "*"

instance Emittable Int where
    emit = fromString . show

instance Emittable CIdent where
    emit ident = "@" <> fromText ident

instance Emittable LLVMIdent where
    emit (LLVMIdent i) = "%" <> emit i

-- Instructions et expressions -------------------------------------------------

data LLVMIdent = LLVMIdent Int

-- | Contient un pointeur vers la variable dans un identifiant.
-- La variable doit être déréférencée ('load') pour être utilisée.
data LLVMVPtr = LLVMVPtr CType LLVMIdent

data LLVMValue = LLVMValueIdent LLVMIdent | LLVMValuePtr LLVMVPtr
               | LLVMValueInt CInt | LLVMValueBool CBool

cCompoundStmt :: CCompoundStmt -> CCodeGen ()
cCompoundStmt = mapM_ cStmt

cStmt :: CStmt -> CCodeGen ()
cStmt (CDecl decl) = cVarDecl decl >>= return ()

cStmt (CAssign varExpr expr) = do left  <- cVarExpr varExpr
                                  right <- cExpr expr
                                  left `store` right

cStmt (CReturn (Just (expr, t)) = do value <- cExpr expr
                                     tellLine $ "ret " <> emit t <-> value
cStmt (CReturn Nothing)         = tellLine "ret void"

cStmt (CExpr expr) = cExpr expr

cStmt (CIf guard ifStmts mElseStmts) = do
    cond <- cExpr guard
    (ifLabel, tellIfLabel) <- label

    -- Les labels doivent être alloués le plus près possible de leur utilisation
    -- ou déclaration, ou l'ordre numérique strictement ascendant des
    -- identifiants ne sera plus respecté.
    tellEndLabel <- case mElseStmts of
        Just elseStmts -> do
            (elseLabel, tellElseLabel) <- label
            branchCond cond ifLabel elseLabel

            tellElseLabel
            cCompoundStmt elseStmts
            (endLabel, tellEndLabel) <- label
            branch endLabel

            return tellEndLabel
        Nothing        -> do
            (endLabel, tellEndLabel) <- label
            branchCond cond ifLabel endLabel

            return tellEndLabel

    tellIfLabel
    cCompoundStmt ifStmts

    tellEndLabel

cStmt (CWhile guard stmts) = do
    (guardLabel, tellGuardLabel) <- label
    tellGuardLabel
    cond <- cExpr guard

    (loopLabel, tellLoopLabel)   <- label
    (endLabel, tellEndLabel)     <- label
    branchCond cond loopLabel endLabel

    tellLoopLabel
    cCompoundStmt stmts
    branch guardLabel

    tellEndLabel

branch :: LLVMIdent -> CCodeGen ()
branch = tellLine . ("br " <>)

branchCond :: LLVMIdent -> LLVMIdent -> LLVMIdent -> CCodeGen ()
branchCond cond ifTrue ifFalse =
    tellLine $ "br i1 " <> emit cond <> ", label " <> ifTrue
                                     <> ", label " <> ifFalse

cVarDecl :: CVarDecl -> CCodeGen LLVMVPtr
cVarDecl (CVarDecl (CVar (CTypeArray _ t _) ident) n mExpr) =
    -- Alloue un espace pour y placer la variable.
    let alloc = "alloca " <> emit t <> ", " <> emit CInt <-> emit n
    ptr <- LLVMVPtr <$> assign alloc
    registerVar ident ptr

    case mExpr of
        Just expr -> cExpr expr >>= store ptr
        Nothing   -> return ()
    return ptr

cExpr :: CExpr -> CCodeGen LLVMValue
cExpr (CCall f args)   = cCall f args
cExpr (CVariable var@(CVarExpr _ dim _)) = do
    ptr <- cVarExpr var
    -- Déréférence le pointeur dans un identifiant si le pointeur pointe vers un
    -- scalaire (i.e. variable ou tableau complètement appliqué).
    case dim of
        CScalar    -> LLVMValueIdent <$> load ptr
        CArray _ _ -> return $ LLVMValuePtr ptr
cExpr (CLitteral litt) = cLitteral litt
cExpr (CBinOp op left right) = do
    left  <- cExpr left
    right <- cExpr right
    LLVMValueIdent <$> instr left right
  where
    instr = case op of CAnd  -> (.&.)
                       COr   -> (.|.)
                       CAdd  -> (.+.)
                       CSub  -> (.-.)
                       CMult -> (.*.)
                       CDiv  -> (./.)
                       CMod  -> (.%.)

-- | Exécute un appel de fonction. Par facilité (pour éviter de manipuler un
-- 'Maybe LLVMIdent' dans l\'arbre d\'expression), un appel à une fonction sans
-- valeur de retour retourne la constante LLVM @0@, même si celle-ci ne sera
-- jamais utilisée.
cCall :: CFun -> CExpr -> CCodeGen LLVMValue
cCall (CFun t ident args _) exprs = do
    -- Calcule les valeurs des arguments.
    values <- mapM cExpr exprs

    let args' = callArgs values
        call  = callType <-> emit ident <> args'
    case t of
        Just prim ->
            assign   $ "call " <> emit prim <-> call
        Nothing   -> do
            tellLine $ "call void "          <> call
            return $ LLVMValueInt 0
  where
    callType =
        let builder = intercalate ", " $ map (emit . getArgType) args
        in "(" <> mconcat builder <> ")*"

    callArgs values =
        let builder = intercalate ", " [ emit (getArgType arg) <-> emit value
                                       | arg <- args | value <- values ]
        in "(" <> mconcat builder <> ")"

-- | Retourne le pointeur correspondant à l'expression.
cVarExpr :: CVarExpr -> CCodeGen LLVMVPtr
cVarExpr (CVarExpr (CVar (CTypeArray _ prim dims) ident) _ subs) = do
    -- Récupère le pointeur associé à la variable.
    Just varPtr@(LLVMVPtr t ptr) <- (ident `M.lookup`) . cgsVarIdents <$> get

    case dims of
        CScalar        -> return varPtr
        CArray _ sizes -> do
            -- Ajoute l'offset si c'est un tableau.
            pad  <- padding sizes subs
            LLVMVPtr t <$> (pad .+. ptr)
  where
    -- Retourne un identifiant qui correspond au résultat du calcul de l'offset.
    padding (size:sizes') (sub:subs) = do
        pad' <- padding sizes' subs -- Offset des dimensions plus profondes.
        pad  <- cExpr sub >>= (LLVMValueInt size .*.)
        pad' .+. pad
    padding []            ~[sub]     = cExpr sub

cLitteral :: CLitteral -> CCodeGen LLVMValue
cLitteral (CLitteralInt i)  = LLVMValueInt  i
cLitteral (CLitteralBool b) = LLVMValueBool b

-- Utilitaires -----------------------------------------------------------------

-- | Exécute l\'opération et enregistre le résultat dans un nouvel identifiant.
-- Retourne ce nouvel identifiant.
assign :: LLVMOp -> CCodeGen LLVMIdent
assign op = do ident <- newIdent
               tellLine $ emit ident <> " = " <> emit op
               return ident

-- | Alloue et retourne un nouvel identifiant.
newIdent :: CCodeGen LLVMIdent
newIdent = state $ \st@(CCodeGenState i _) ->
    (LLVMIdent i, st { cgsNextIdent = i + 1 })

-- | Déréférence la variable en chargeant son contenu dans un identifiant.
load :: LLVMVariable -> CCodeGen LLVMIdent
load (LLVMVariable t ptr) = assign $ "load " <> emit t <> "* " <> emit ptr

-- | Alloue un nouveau label sans l\'émettre (pour permettre son utilisation
-- avant sa déclaration). Retourne une action qui permettra de le déclarer.
label :: CCodeGen (LLVMIdent, CCodeGen ())
label = do ident@(LLVMIdent i) <- newIdent
           return (ident, tellLine $ "; <label>:" <> emit i)

-- | Enregistre une valeur dans une variable.
store :: LLVMVariable -> LLVMValue -> CCodeGen ()
store (LLVMVariable t ptr) val =
    let t' = emit t
    in "store " <> t' <-> emit val <> ", " <> t' <> "* " <> emit ptr

(.&.), (.|.), (.+.), (.-.), (.*.), (./.), (.%.) ::
    LLVMValue -> LLVMValue -> CCodeGen LLVMIdent
((.&.), (.|.), (.+.), (.-.), (.*.), (./.), (.%.)) = (
      binOp "and" CBool, binOp "or" CBool
    , intOp "add", intOp "sub", intOp "mult", intOp "div", intOp "mod"
    )
  where
    binOp opCode t a b = assign $ opCode <-> emit t <-> emit a <> ", " <> emit b

    intOp opCode = binOp opCode CInt

-- | Retiens la correspondance entre l\'identificateur de la variable et le
-- pointeur.
registerVar :: CIdent -> LLVMPtr -> CCodeGen ()
registerVar ident ptr =
    modify $ \st -> st { cgsVarPtrs = M.insert ident ptr (cgsVarPtrs st) })

-- | Concatène deux 'Builder' en ajoutant un espace entre les deux.
(<->) :: Builder -> Builder -> Builder
a <-> b = a <> " " <> b

-- | Emet une nouvelle instruction IR suivie d\'un retour à la ligne.
tellLine :: Builder -> CCodeGen ()
tellLine = tell . (<> "\n")
