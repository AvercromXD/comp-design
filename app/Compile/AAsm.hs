module Compile.AAsm
 ( codeGen
 ) where

import Compile.AST (AST(..), Expr(..), Stmt(..), Op, showAsgnOp)
import Compile.AAAST (AAAST(..), Inst(..), Operand(..))
import Control.Monad.State
import qualified Data.Map as Map

type VarName = String
type Register = Integer
type RegisterMap = Map.Map VarName Register
type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
 { regMap :: RegisterMap
 , nextReg :: Register
 , code :: [Inst]
 }

-- | Main entry point for code generation
codeGen :: AST -> AAAST
codeGen (Compile.AST.Block stmts _) = Compile.AAAST.Block $ code $ execState (genBlock stmts) initialState
  where
    initialState = CodeGenState Map.empty 0 []

-- | Generate a fresh register
freshReg :: CodeGen Register
freshReg = do
  curr <- get
  let r = nextReg curr
  put curr {nextReg = r + 1}
  return r

-- | Assign a register to a variable
assignVar :: VarName -> Register -> CodeGen ()
assignVar name op = do
  modify $ \s -> s {regMap = Map.insert name op (regMap s)}

-- | Look up a variable's operand
lookupVar :: VarName -> CodeGen Register
lookupVar name = do
  m <- gets regMap
  maybe freshReg return (Map.lookup name m)

-- | Add an instruction to the code
emit :: Inst -> CodeGen ()
emit inst = modify $ \s -> s {code = code s ++ [inst]}

-- | Process a block of statements
genBlock :: [Stmt] -> CodeGen ()
genBlock = mapM_ genStmt

-- | Process a statement
genStmt :: Stmt -> CodeGen ()
genStmt (Decl name _) = do
  r <- freshReg
  emit $ Compile.AAAST.Init r $ Con 0
  assignVar name r

genStmt (Compile.AST.Init name e _) = do
  op <- genExpr e
  r <- freshReg
  emit $ Compile.AAAST.Init r op
  assignVar name r

genStmt (Compile.AST.Asgn name (Just op) e _) = do
  rhs <- genExpr e
  r <- lookupVar name

  -- Maximum munch for assignment
  case (r, rhs) of
    -- When both sides are operands, emit a direct assignment
    (_, _) ->
      -- For simplicity, we'll use a base case that handles everything
      -- In a full maximum munch implementation, you would add specific pattern matches here
      emit $ Compile.AAAST.Asgn r op rhs

genStmt (Compile.AST.Asgn name Nothing e _) = do
  rhs <- genExpr e
  r <- lookupVar name
  emit $ Compile.AAAST.Init r rhs


genStmt (Compile.AST.Ret e _) = do
  -- Handle return statement (could be adapted based on your target language)
  -- For now, we'll just evaluate the expression
  rhs <- genExpr e
  emit $ Compile.AAAST.Ret rhs

-- | Process an expression using maximum munch
genExpr :: Expr -> CodeGen Operand
genExpr (IntExpr n _) =
  -- Constants are represented directly
  return $ Con n

genExpr (Ident name _) = do
  r <- lookupVar name
  return $ Reg r

genExpr (UnExpr op e) = do
  opnd <- genExpr e
  -- For unary operations, we can use a dummy second operand (or adapt the AAAST to support unary ops)
  -- Using Con 0 as a placeholder, but this should be adapted to your needs
  case opnd of
    Reg r -> 
      emit $ UnOpAsgn r op    
    Con i -> do
      r <- freshReg
      emit $ Compile.AAAST.Init r (Con i)
      emit $ UnOpAsgn r op
  return opnd

genExpr (BinExpr op e1 e2) = do
  opnd1 <- genExpr e1
  opnd2 <- genExpr e2
  r <- freshReg

  -- Maximum munch for binary operations
  -- Here we can add specific patterns for optimal instruction selection
  -- For now, we'll use a general case
  emit $ Compile.AAAST.Init r opnd1
  emit $ Compile.AAAST.Asgn r op opnd2
  return $ Reg r
