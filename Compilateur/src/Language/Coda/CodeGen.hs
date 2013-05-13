{-# LANGUAGE FlexibleInstances, OverloadedStrings, ParallelListComp #-}
-- | Définit le générateur de code qui génère le code IR d\'LLVM à partir de
-- l\'arbre syntaxique du programme.
module Language.Coda.CodeGen (genCode) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.State (State, evalState, get, modify, state)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Monoid ((<>), mconcat)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, fromString, fromText, toLazyText)

import Language.Coda.AST

-- | Monade utilisée lors de la génération des fonctions pour émettre du code
-- IR, pour garder le comptage des identifiants et pour retenir la
-- correspondance entre les identifiants des variables de l\''AST' et ceux du
-- LLVM IR.
type CCodeGen = WriterT Builder (State CCodeGenState)

data CCodeGenState = CCodeGenState {
      cgsNextReg :: !LLVMReg, cgsNextLabel :: !LLVMLabel
    , cgsVarPtrs :: M.Map CIdent LLVMPtr
    }

genCode :: AST -> TL.Text
genCode = toLazyText . mconcat . map cFun . astFuns

-- Types liés au générateur LLVM -----------------------------------------------

-- | Registre LLVM, soit nommé automatiquement ('LLVMRegInt'), soit nommé 
-- explicitement.
data LLVMReg = LLVMRegInt Int | LLVMRegText Text

-- | Les labels identifient une section du code généré.
newtype LLVMLabel = LLVMLabel Int

-- | Contient un pointeur vers la variable dans un registre.
-- La variable doit être déréférencée ('load') pour être utilisée.
data LLVMPtr = LLVMPtr CType LLVMReg

-- | Représente les valeurs qui peuvent être utilisées comme opérande d\'un
-- opérateur d\'LLVM
data LLVMValue = LLVMValueReg LLVMReg | LLVMValuePtr LLVMPtr
               | LLVMValueInt CInt | LLVMValueBool CBool

-- | Type class utilitaire permettant d\'afficher sur les valeurs sur le flux
-- de sortie du compilateur.
class Emittable a where
    emit :: a -> Builder

instance Emittable CIdent where
    emit = fromText . ciIdent

instance Emittable CType where
    emit CInt  = "i64"
    emit CBool = "i1"

instance Emittable (Maybe CType) where
    emit (Just t) = emit t
    emit Nothing  = "void"

instance Emittable CTypeArray where
    emit (CTypeArray _ prim CScalar)      = emit prim
    emit (CTypeArray _ prim (CArray _ _)) = emit prim <> "*"

instance Emittable Int where
    emit = fromString . show

instance Emittable CInt where
    emit = fromString . show

instance Emittable LLVMReg where
    emit (LLVMRegInt i)      = "%" <> emit i
    emit (LLVMRegText ident) = "%" <> fromText ident

instance Emittable LLVMLabel where
    emit (LLVMLabel i) = "label %label_" <> emit i

instance Emittable LLVMPtr where
    emit (LLVMPtr _ ident) = emit ident

instance Emittable LLVMValue where
    emit (LLVMValueReg reg)    = emit reg
    emit (LLVMValuePtr ptr)    = emit ptr
    emit (LLVMValueInt i)      = emit i
    emit (LLVMValueBool True)  = "true"
    emit (LLVMValueBool False) = "false"

-- Fonctions -------------------------------------------------------------------

cFun :: CFun -> Builder
cFun (CFun ret ident (CArgs args) mStmts) =
    let fType = emit ret <> " @" <> emit ident
    in case mStmts of
        Just stmts ->
            let args' = map stackArg args
                instrs = runCodeGen $ do
                    mapM_ snd args' -- Copie des arguments scalaires sur la pile
                    cCompoundStmt stmts -- Génère la fonction

                    case ret of Nothing -> tellLine "ret void"
                                _       -> return ()
            in "define " <> fType <> argList (map fst args') <> " {\n"
                         <> instrs <> "}\n"
        Nothing -> "declare " <> fType <> argList (map (emit . getArgType) args)
  where
    runCodeGen = flip evalState emptyState . execWriterT

    -- Retourne la déclaration d'un argument ainsi qu'une action qui sera
    -- exécutée en début de génération de la fonction pour copier les arguments
    -- scalaires sur la pile et référencer directement les tableaux par leur
    -- pointeur (passage par référence).
    stackArg :: CArg -> (Builder, CCodeGen ())
    stackArg (CVarArg var@(CVar t@(CTypeArray _ prim dims) varIdent)) =
        let reg = LLVMRegText $ ciIdent varIdent
            action  = case dims of
                CScalar    -> do
                    ptr <- cVarDecl $ CVarDecl var 1 Nothing
                    ptr `store` LLVMValueReg reg
                CArray _ _ -> registerVar varIdent (LLVMPtr prim reg)
        in (emit t <-> emit reg, action)
    stackArg (CAnonArg t) = (emit t, return ())

    emptyState = CCodeGenState (LLVMRegInt 1) (LLVMLabel 1) M.empty

-- Instructions et expressions -------------------------------------------------

cCompoundStmt :: CCompoundStmt -> CCodeGen ()
cCompoundStmt = mapM_ cStmt . csStmts

cStmt :: CStmt -> CCodeGen ()
cStmt (CDecl decl) = cVarDecl decl >> return ()

cStmt (CAssign varExpr expr) = do left  <- cVarExpr varExpr
                                  right <- cExpr expr
                                  left `store` right

cStmt (CReturn (Just (expr, t))) = do value <- cExpr expr
                                      tellLine $ "ret " <> emit t <-> emit value
cStmt (CReturn Nothing)          = tellLine "ret void"

cStmt (CExpr expr) = cExpr expr >> return ()

cStmt (CIf guard ifStmts mElseStmts) = do
    cond <- cExpr guard
    ifLabel  <- newLabel
    endLabel <- newLabel

    elseRet <- case mElseStmts of
        Just elseStmts -> do
            elseLabel <- newLabel
            branchCond cond ifLabel elseLabel

            tellLabel elseLabel
            cCompoundStmt elseStmts
            if csReturn elseStmts then return True
                                  else branch endLabel >> return False
        Nothing        -> do
            branchCond cond ifLabel endLabel >> return False

    tellLabel ifLabel
    cCompoundStmt ifStmts
    let ifRet = csReturn ifStmts
    when (not ifRet) $ branch endLabel

    when (not $ ifRet && elseRet) $ tellLabel endLabel

cStmt (CWhile guard stmts) = do
    guardLabel <- newLabel
    loopLabel  <- newLabel
    endLabel   <- newLabel
    branch guardLabel

    tellLabel guardLabel
    cond <- cExpr guard
    branchCond cond loopLabel endLabel

    tellLabel loopLabel
    cCompoundStmt stmts
    when (not $ csReturn stmts) $ branch guardLabel

    tellLabel endLabel

cVarDecl :: CVarDecl -> CCodeGen LLVMPtr
cVarDecl (CVarDecl (CVar (CTypeArray _ prim _) ident) n mExpr) = do
    -- Alloue un espace pour y placer la variable.
    let alloc = "alloca " <> emit prim <> ", " <> emit CInt <-> emit n
    ptr <- LLVMPtr prim <$> assign alloc
    registerVar ident ptr

    case mExpr of
        Just expr -> cExpr expr >>= store ptr
        Nothing   -> return ()
    return ptr

cExpr :: CExpr -> CCodeGen LLVMValue
cExpr (CCall f args) =
    -- Par facilité, un appel à une fonction sans valeur de retour retourne la
    -- constante LLVM 0, même si celle-ci ne sera jamais utilisée.
    maybe (LLVMValueInt 0) LLVMValueReg <$> cCall f args
cExpr (CVariable var@(CVarExpr _ dim _)) = do
    ptr <- cVarExpr var
    -- Déréférence le pointeur dans un identifiant si l'expression retourne un
    -- pointeur vers un scalaire (i.e. variable ou tableau complètement
    -- appliqué).
    case dim of
        CScalar    -> LLVMValueReg <$> load ptr
        CArray _ _ -> return $ LLVMValuePtr ptr
cExpr (CLitteral litt) = return $ cLitteral litt
cExpr (CBinOp op left right) = do
    left'  <- cExpr left
    right' <- cExpr right
    LLVMValueReg <$> instr left' right'
  where
    instr = case op of CAnd  -> (.&.)
                       COr   -> (.|.)
                       CEq   -> (.==.)
                       CNEq  -> (.!=.)
                       CLt   -> (.<.)
                       CGt   -> (.>.)
                       CLtEq -> (.<=.)
                       CGtEq -> (.>=.)
                       CAdd  -> (.+.)
                       CSub  -> (.-.)
                       CMult -> (.*.)
                       CDiv  -> (./.)
                       CMod  -> (.%.)

-- | Exécute un appel de fonction.
cCall :: CFun -> [CExpr] -> CCodeGen (Maybe LLVMReg)
cCall (CFun t ident (CArgs args) _) exprs = do
    -- Calcule les valeurs des arguments.
    values <- mapM cExpr exprs

    let call  = callType <-> "@" <> emit ident <> args'
        args' = callArgs values
    case t of
        Just prim ->
            Just <$> (assign $ "call " <> emit prim <-> call)
        Nothing   -> do
            tellLine $ "call void " <> call
            return Nothing
  where
    callType = argList (map (emit . getArgType) args) <> "*"

    callArgs values = argList [ emit (getArgType arg) <-> emit value
                              | arg <- args | value <- values ]

-- | Retourne le pointeur correspondant à l'expression.
cVarExpr :: CVarExpr -> CCodeGen LLVMPtr
cVarExpr (CVarExpr (CVar (CTypeArray _ prim dims) ident) _ subs) = do
    -- Récupère le pointeur associé à la variable.
    Just varPtr <- ((ident `M.lookup`) . cgsVarPtrs) <$> get

    case dims of
        CScalar        -> return varPtr
        CArray _ sizes | null subs -> return varPtr
                       | otherwise -> do
            -- Ajoute l'offset si subscript est un tableau.
            pad <- padding sizes subs
            ptr <- assign $ "getelementptr " <> emit prim <> "* " <> emit varPtr
                                             <> ", " <> emit CInt <-> emit pad
            return $ LLVMPtr prim ptr
  where
    -- Retourne une valeur correspond au résultat du calcul de l'offset.
    padding (size:sizes') ~(sub:subs') = do
        pad' <- padding sizes' subs' -- Offset des dimensions plus profondes.
        pad  <- cExpr sub >>= (LLVMValueInt size .*.)
        LLVMValueReg <$> (pad' .+. LLVMValueReg pad)
    padding []            ~[sub]       = cExpr sub

cLitteral :: CLitteral -> LLVMValue
cLitteral (CLitteralInt i)  = LLVMValueInt  i
cLitteral (CLitteralBool b) = LLVMValueBool b

-- Utilitaires et primitives LLVM ----------------------------------------------

-- | Concatène les 'Builder's en une liste entre parenthèses.
argList :: [Builder] -> Builder
argList args = "(" <> (mconcat (intersperse ", " args)) <> ")"

-- | Assigne l\'opération dans un nouveau registre. Retourne l\'identifiant de
-- ce nouveau registre.
assign :: Builder -> CCodeGen LLVMReg
assign op = do reg <- newReg
               tellLine $ emit reg <> " = " <> op
               return reg

-- | Branche sur le bloc inconditionnellement.
branch :: LLVMLabel -> CCodeGen ()
branch = tellLine . ("br " <>) . emit

-- | Branche en fonction de la valeur d'une condition.
branchCond :: LLVMValue -> LLVMLabel -> LLVMLabel -> CCodeGen ()
branchCond cond ifTrue ifFalse =
    tellLine $ "br i1 " <> emit cond <> ", " <> emit ifTrue
                                     <> ", " <> emit ifFalse

-- | Alloue un nouveau label sans l\'émettre (pour permettre son utilisation
-- avant sa déclaration).
newLabel :: CCodeGen LLVMLabel
newLabel = state $ \st@(CCodeGenState _ label@(LLVMLabel i) _) ->
    (label, st { cgsNextLabel = LLVMLabel (i + 1) })

-- | Alloue et retourne un nouvel identifiant numérique.
newReg :: CCodeGen LLVMReg
newReg = state $ \st@(CCodeGenState reg@(LLVMRegInt i) _ _) ->
    (reg, st { cgsNextReg = LLVMRegInt (i + 1) })

-- | Déréférence la variable en chargeant son contenu dans un identifiant.
load :: LLVMPtr -> CCodeGen LLVMReg
load (LLVMPtr t ptr) = assign $ "load " <> emit t <> "* " <> emit ptr

-- | Retiens la correspondance entre l\'identificateur de la variable et le
-- pointeur.
registerVar :: CIdent -> LLVMPtr -> CCodeGen ()
registerVar ident ptr =
    modify $ \st -> st { cgsVarPtrs = M.insert ident ptr (cgsVarPtrs st) }

-- | Enregistre une valeur dans une variable.
store :: LLVMPtr -> LLVMValue -> CCodeGen ()
store (LLVMPtr t ptr) val =
    let t' = emit t
    in tellLine $ "store " <> t' <-> emit val <> ", " <> t' <> "* " <> emit ptr

-- | Indique le début d\'un nouveau bloc identifié par un label.
tellLabel :: LLVMLabel -> CCodeGen ()
tellLabel (LLVMLabel i) = tell $ "label_" <> emit i <> ":\n"

-- | Emet une nouvelle instruction IR suivie d\'un retour à la ligne.
tellLine :: Builder -> CCodeGen ()
tellLine = tell . ("    " <>) . (<> "\n")

-- Opcodes arithmétiques et relationnels d'LLVM.

(.&.), (.|.), (.==.), (.!=.), (.<.), (.>.), (.<=.), (.>=.), (.+.), (.-.), (.*.),
    (./.), (.%.) :: LLVMValue -> LLVMValue -> CCodeGen LLVMReg
((.&.), (.|.), (.==.), (.!=.), (.<.), (.>.), (.<=.), (.>=.),(.+.), (.-.), (.*.),
 (./.), (.%.)) = (
      binOp "and" CBool, binOp "or" CBool
    , icmp "eq", icmp "ne", icmp "slt", icmp "sgt", icmp "sle", icmp "sge"
    , intOp "add", intOp "sub", intOp "mul", intOp "sdiv", intOp "srem"
    )
  where
    binOp opCode t a b = assign $ opCode <-> emit t <-> emit a <> ", " <> emit b

    icmp cond = binOp ("icmp" <-> cond) CInt
    intOp opCode = binOp opCode CInt

-- | Concatène deux 'Builder' en ajoutant un espace entre les deux.
(<->) :: Builder -> Builder -> Builder
a <-> b = a <> " " <> b
