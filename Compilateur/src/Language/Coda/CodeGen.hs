{-# LANGUAGE OverloadedStrings #-}
-- | Définit le générateur de code qui génère le code IR d\'LLVM à partir de
-- l\'arbre syntaxique du programme.
module Language.Coda.CCodeGen (genCode, codeGen) where

import Control.Monad.State (State, get, put, state)
import Data.Monoid ((<>))
import Control.Monad.Writer (WriterT, tell)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)

import Language.Code.AST

type CCodeGen = WriterT TL.Builder (State CCodeGenState)
-- type CCodeGen = State LLVMIdent

newtype LLVMIdent = LLVMIdent Int

data CCodeGenState = CCodeGenState {
      cgsNextIdent :: !Int, cgsVarIdents :: M.Map CIdent LLVMVariable
    }

let emptyState = CCodeGenState 1 M.empty

genCode :: AST -> TL.Text
genCode = TL.unlines . map (TL.takeWhile (/= '#')) . TL.lines

CCodeGen a -> 
runCCodeGen = 

codeGen :: AST -> CCodeGen ()


-- Fonctions -------------------------------------------------------------------

cFun :: CFun -> CCodeGen Builder
cFun (CFun ret ident args mStmts) = do
    let ret' | Just t <- ret = cType t
             | otherwise     = "void"
        ident' = "@" <> ident
    args' <- 

    let fType = ret' <> " " <> ident' <> args'
    return $ case mStmts of
        Just stmts -> "define "  <> fType <> stmts'
        Nothing    -> "declare " <> fType

-- ; Declare the string constant as a global constant.
-- @.str = global [13 x i8] c"hello world\0A\00"
-- 
-- ; Definition of main function
-- define i32 @main() {   ; i32()*
--   ; Convert [13 x i8]* to i8  *...
--   %cast210 = getelementptr [13 x i8]* @.str, i64 0, i64 0
-- 
--   ; Call puts function to write out the string to stdout.
--   call i32 @puts(i8* %cast210)
--   ret i32 0
-- }
-- ; External declaration of the puts function
-- declare i32 @puts(i8*)


    

-- Types -----------------------------------------------------------------------

class Emitable a where
    emit :: a -> Builder

instance Emitable CType where
    emit CInt  = "i64"
    emit CBool = "i1"

instance Emitable LLVMIdent where
    emit (LLVMIdent i) = "%" <> (fromString $ show i)


cTypeArray :: CTypeArray -> Builder
cTypeArray =

cArguments 

-- Instructions et expressions -------------------------------------------------

data LLVMIdent = LLVMIdent Int

data LLVMValue = LLVMValueIdent LLVMIdent
               | LLVMValueInt CInt | LLVMValueBool CBool

newtype LLVMOp = LLVMOp Builder

ret :: Maybe (CType, LLVMIdent) -> LLVMIdent -> CCodeGen ()

<- add (LLVMLitteral 10) (LLVMLitteral 20)

<- assign $ load 10

ret :: CType -> LLVMIdent -> LLVMOpCode
ret 

cExpr :: CExpr -> CCodeGen LLVMValue
cExpr (CCall f args)   = cCall f args
cExpr (CVariable var)  = LLVMValueIdent <$> (cVarExpr var >>= cload)
cExpr (CLitteral litt) = cLitteral litt
cExpr (CBinOp op left right) =
    LLVMValueIdent <$> (instr <*> cExpr left <*> cExpr right)
  where
    instr = case op of CAnd  -> cand
                       COr   -> cor
                       CAdd  -> cadd
                       CSub  -> csub
                       CMult -> cmult
                       CDiv  -> cdiv
                       CMod  -> cmod

cCall :: CFun -> CExpr -> CCodeGen (Maybe LLVMIdent)
cCall f args = do
    assign < "call " <
    call i32 (i8*, ...)* @printf(i8* @fmt, i32 %val)

-- | Contient un pointeur vers la variable dans un identifiant.
-- La variable doit être déréférencée ('load') pour être utilisée.
data LLVMVariable = LLVMVariable CType LLVMIdent

-- | Retourne le pointeur correspondant à l'expression.
cVarExpr :: CVarExpr -> CCodeGen LLVMVariable
cVarExpr (CVarExpr (CVar (CTypeArray _ CType dims) ident) subs) = do
    -- Récupère le pointeur associé à la variable.
    Just var@(LLVMVariable t ptr) <- (ident `M.lookup`) . cgsVarIdents <$> get

    case dims of
        CScalar        -> return varPtr
        CArray _ sizes -> do
            -- Ajoute l'offset si c'est un tableau.
            pad  <- padding sizes subs
            LLVMVariable t <$> cadd pad varPtr
  where
    -- Retourne un identifiant qui correspond au résultat du calcul de l'offset.
    padding (size:sizes') (sub:subs) = do
        pad' <- padding sizes' subs -- Offset des dimensions plus profondes.
        pad  <- cExpr sub >>= cmult (LLVMValueInt size)
        cadd pad' pad
    padding []            ~[sub]     = cExpr sub

cLitteral :: CLitteral -> CCodeGen LLVMValue
cLitteral (CLitteralInt i)  = LLVMValueInt  i
cLitteral (CLitteralBool b) = LLVMValueBool b

-- Utilitaires -----------------------------------------------------------------

-- | Exécute l\'opération et enregistre le résultat dans un nouvel identifiant.
-- Retourne ce nouvel identifiant.
assign :: LLVMOp -> CCodeGen LLVMIdent
assign op = do
    ident <- newIdent
    tellLine $ emit ident <> " = " <> emit op
    return ident

-- | Alloue et retourne un nouvel identifiant.
newIdent :: CCodeGen LLVMIdent
newIdent = state $ \st@(CCodeGenState i _) ->
    (LLVMIdent i, st { cgsNextIdent = i + 1 })

-- | Déréférence la variable en chargeant son contenu dans un identifiant.
cload :: LLVMVariable -> CCodeGen LLVMIdent
cload (LLVMVariable t ptr) = assign $ "load " <> emit t <> "* " <> emit ptr

-- | Enregistre une valeur dans une variable.
cstore :: LLVMVariable -> LLVMValue -> CCodeGen ()
cstore (LLVMVariable t ptr) val =
    let t' = emit t
    in "store " <> t' <> " " <> emit val <> ", " <> t' <> "* " <> emit ptr

cand, cor, cadd, csub, cmult, cdiv, cmod ::
    LLVMValue -> LLVMValue -> CCodeGen LLVMIdent
(cand, coor, cadd, csub, cmult, cdiv, cmod) = (
      binOp "and" CBool, binOp "or" CBool
    , intOp "add", intOp "sub", intOp "mult", intOp "div", intOp "mod"
    )
  where
    binOp opCode t a b =
        assign $ opCode <> " " <> emit t <> " " <> emit a <> " " <> emit b

    intOp opCode = binOp opCode CInt

-- | Emet une nouvelle instruction IR suivie d\'un retour à la ligne.
tellLine :: Builder -> CCodeGen ()
tellLine = tell . (<> "\n")
