{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Compile.RegisterAllocation
  ( allocateRegisters,
    colorVariables,
    usableFromIdx,
  )
where

import Compile.AAAST (AAAST (Block), Inst (..), Operand (..))
import Compile.AST (Op (..))
import Compile.GraphColoring (Edge, buildgraph, coloring)
import Compile.Liveness (LiveRegisters, Register)
import Control.Monad.State (State, execState, gets, modify)
import qualified Data.HashSet as HS
import qualified Data.Map as Map

data X86_64Register
  = Rax -- Used for storing quotient during division and return value
  | Rbx
  | Rcx
  | Rdx -- Stores remainder during division
  | Rsi
  | Rdi
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14 -- Source register for spilling
  | R15 -- Detination register for spilling
  | Rsp -- Stack pointer
  | Rbp -- Base pointer
  | Spilled Int -- Spilled register
  deriving (Eq, Ord)

instance Show X86_64Register where
  show :: X86_64Register -> String
  show Rax = "%rax"
  show Rbx = "%rbx"
  show Rcx = "%rcx"
  show Rdx = "%rdx"
  show Rsi = "%rsi"
  show Rdi = "%rdi"
  show R8 = "%r8"
  show R9 = "%r9"
  show R10 = "%r10"
  show R11 = "%r11"
  show R12 = "%r12"
  show R13 = "%r13"
  show R14 = "%r14"
  show R15 = "%r15"
  show Rsp = "%rsp"
  show Rbp = "%rbp"
  show (Spilled _) = error "Spilled register not supported in this context"

-- | Size of registers in bytes
regSizeB :: Int
regSizeB = 8

usableRegisters :: [X86_64Register]
usableRegisters =
  [ Rbx,
    Rcx,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13
  ]

usableFromIdx :: (Integral a, Num a) => a -> X86_64Register
usableFromIdx n =
  if fromIntegral n < length usableRegisters
    then usableRegisters !! fromIntegral n
    else Spilled (fromIntegral n - length usableRegisters)

data Operations
  = ADD -- add S, D Add source to destination
  | SUB -- sub S, D Subtract source from destination
  | MUL -- mul S, D Multiply source with destination
  | DIV -- div S, D Divide source by destination, quotient in Rax, remainder in Rdx
  | NEG -- neg D Negate destination
  | MOV -- mov S, D Move source to destination
  | PUSH -- push S Push source onto stack
  | POP -- pop D Pop top of stack into destination
  | RET -- ret Return from function
  | CLTD -- cltd Sign extend EAX into EDX:EAX
  deriving (Eq)

data Direction
  = Src -- Source operand
  | Dst -- Destination operand
  deriving (Eq)

-- | Show instance for Operations on 32-bit integers
instance Show Operations where
  show :: Operations -> String
  show ADD = "addq"
  show SUB = "subq"
  show MUL = "imulq"
  show DIV = "idivq"
  show NEG = "negq"
  show MOV = "movq"
  show PUSH = "pushq"
  show POP = "popq"
  show RET = "ret"
  show CLTD = "cqo"

srcSpillReg :: X86_64Register
srcSpillReg = R14

dstSpillReg :: X86_64Register
dstSpillReg = R15

makeImm :: String -> String
makeImm n = "$" ++ n

type RegisterMap = Map.Map Integer X86_64Register

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
  { regMap :: RegisterMap,
    code :: [String]
  }

allocateRegisters :: AAAST -> [LiveRegisters] -> [String]
allocateRegisters (Block inst) liveRegs = code $ execState (genBlock (filter (`filterLiveInsts` registerMap) inst)) initialState
  where
    initialState
      | numSpilledRegisters registerMap /= 0 = CodeGenState registerMap [show SUB ++ " " ++ makeImm (show ((numSpilledRegisters registerMap + 1) * regSizeB)) ++ ", " ++ show Rsp]
      | otherwise = CodeGenState registerMap []
    registerMap = colorVariables liveRegs

filterLiveInsts :: Inst -> RegisterMap -> Bool
filterLiveInsts (Init d _) m = Map.member d m
filterLiveInsts (Asgn d _ _ _) m = Map.member d m
filterLiveInsts (Ret _) _ = True

emit :: String -> CodeGen ()
emit s = modify $ \s' -> s' {code = code s' ++ [s]}

numSpilledRegisters :: RegisterMap -> Int
numSpilledRegisters m = length $ filter isSpilled (Map.elems m)

genBlock :: [Inst] -> CodeGen ()
genBlock = mapM_ genInst

lookupReg :: Register -> CodeGen X86_64Register
lookupReg reg = do
  m <- gets regMap
  case Map.lookup reg m of
    Just r -> return r
    Nothing -> error $ "Register " ++ show reg ++ " not found in register map"

-- | Retrieve the value from the stack into the temp registers %r14/%r15
-- mov (n+1)*regSizeB(%rsp), %r14/%r15
retrieveFromStack :: Direction -> X86_64Register -> CodeGen ()
retrieveFromStack Src (Spilled n) = do
  emit $ show MOV ++ " " ++ show ((n + 1) * regSizeB) ++ "(" ++ show Rsp ++ ")" ++ ", " ++ show srcSpillReg
retrieveFromStack Dst (Spilled n) = do
  emit $ show MOV ++ " " ++ show ((n + 1) * regSizeB) ++ "(" ++ show Rsp ++ ")" ++ ", " ++ show dstSpillReg
retrieveFromStack _ _ = error "Not a spilled register"

-- | Store the value of temp register %r14/%r15 to the stack
-- mov %r15/%r15, (n+1)*regSizeB(%rsp)
storeToStack :: X86_64Register -> CodeGen ()
storeToStack (Spilled n) = do
  emit $ show MOV ++ " " ++ show dstSpillReg ++ ", " ++ show ((n + 1) * regSizeB) ++ "(" ++ show Rsp ++ ")"
storeToStack _ = error "Not a spilled register"

loadSrcRegister :: Register -> CodeGen X86_64Register
loadSrcRegister n = do
  srcReg <- lookupReg n
  case srcReg of
    Spilled s -> do
      retrieveFromStack Src (Spilled s)
      return srcSpillReg
    _ -> return srcReg

loadDstRegister :: Register -> CodeGen X86_64Register
loadDstRegister n = do
  destReg <- lookupReg n
  case destReg of
    Spilled _ -> do
      return dstSpillReg
    _ -> return destReg

storeRegister :: Register -> CodeGen ()
storeRegister n = do
  dstReg <- lookupReg n
  case dstReg of
    Spilled s -> do
      storeToStack (Spilled s)
      return ()
    _ -> do
      return ()

isSpilled :: X86_64Register -> Bool
isSpilled (Spilled _) = True
isSpilled _ = False

genInst :: Inst -> CodeGen ()
genInst (Init dest (Reg src)) = binOp' MOV src dest
genInst (Init dest (Con src)) = immOp' MOV src dest
genInst (Asgn dest (Reg src1) op (Reg src2)) = case op of
  Compile.AST.Add -> binOp ADD src1 src2 dest
  Compile.AST.Sub -> binOp SUB src1 src2 dest
  Compile.AST.Mul -> binOp MUL src1 src2 dest
  Compile.AST.Div -> divModOp DivResult src1 src2 dest
  Compile.AST.Mod -> divModOp ModResult src1 src2 dest
  _ -> error "Unsupported operation"
genInst (Asgn dest (Reg src1) op (Con src2)) = case op of
  Compile.AST.Add -> immRegOp ADD src2 src1 dest
  Compile.AST.Sub -> immRegOp SUB src2 src1 dest
  Compile.AST.Mul -> immRegOp MUL src2 src1 dest
  Compile.AST.Div -> error "Division cannot use immediate operands"
  Compile.AST.Mod -> error "Modulo cannot use immediate operands"
  _ -> error "Unsupported operation"
genInst (Asgn dest (Con src1) op (Reg src2)) = case op of
  Compile.AST.Add -> immRegOp ADD src1 src2 dest
  Compile.AST.Sub -> immRegOp SUB src1 src2 dest
  Compile.AST.Mul -> immRegOp MUL src1 src2 dest
  Compile.AST.Div -> error "Division cannot use immediate operands"
  Compile.AST.Mod -> error "Modulo cannot use immediate operands"
  _ -> error "Unsupported operation"
genInst (Asgn dest (Con src1) op (Con src2)) = case op of
  Compile.AST.Add -> immOp ADD src1 src2 dest
  Compile.AST.Sub -> immOp SUB src1 src2 dest
  Compile.AST.Mul -> immOp MUL src1 src2 dest
  Compile.AST.Div -> error "Division cannot use immediate operands"
  Compile.AST.Mod -> error "Modulo cannot use immediate operands"
  _ -> error "Unsupported operation"
genInst (Ret (Reg src)) = do
  m <- gets regMap
  let hasSpilledRegisters = numSpilledRegisters m > 0
  srcReg <- loadSrcRegister src
  if hasSpilledRegisters
    then do
      emit $ show MOV ++ " " ++ show srcReg ++ ", " ++ show Rax
      let stackSize = (numSpilledRegisters m + 1) * regSizeB
      emit $ show ADD ++ " " ++ makeImm (show stackSize) ++ ", " ++ show Rsp
      emit $ show RET
    else do
      emit $ show MOV ++ " " ++ show srcReg ++ ", " ++ show Rax
      emit $ show RET
genInst (Ret (Con src)) = do
  m <- gets regMap
  let hasSpilledRegisters = numSpilledRegisters m > 0
  if hasSpilledRegisters
    then do
      emit $ show MOV ++ " " ++ makeImm src ++ ", " ++ show Rax
      let stackSize = (numSpilledRegisters m + 1) * regSizeB
      emit $ show ADD ++ " " ++ makeImm (show stackSize) ++ ", " ++ show Rsp
      emit $ show RET
    else do
      emit $ show MOV ++ " " ++ makeImm src ++ ", " ++ show Rax
      emit $ show RET

-- Result type for division/modulo
data DivModResult = DivResult | ModResult

binOp :: Operations -> Register -> Register -> Register -> CodeGen ()
binOp op src1 src2 dest = do
  src1Reg <- loadSrcRegister src1
  destReg <- loadDstRegister dest
  emit $ show MOV ++ " " ++ show src1Reg ++ ", " ++ show destReg
  src2Reg <- loadSrcRegister src2
  emit $ show op ++ " " ++ show src2Reg ++ ", " ++ show destReg
  storeRegister dest

binOp' :: Operations -> Register -> Register -> CodeGen ()
binOp' op src dest = do
  srcReg <- loadSrcRegister src
  destReg <- loadDstRegister dest
  emit $ show op ++ " " ++ show srcReg ++ ", " ++ show destReg
  storeRegister dest

divModOp :: DivModResult -> Register -> Register -> Register -> CodeGen ()
divModOp resultType src1 src2 dest = do
  destReg <- loadDstRegister dest
  src1Reg <- loadSrcRegister src1
  emit $ show MOV ++ " " ++ show src1Reg ++ ", " ++ show Rax
  emit $ show CLTD
  src2Reg <- loadSrcRegister src2
  emit $ show DIV ++ " " ++ show src2Reg
  emit $ show MOV ++ " " ++ show (case resultType of DivResult -> Rax; ModResult -> Rdx) ++ ", " ++ show destReg
  storeRegister dest

immRegOp :: Operations -> String -> Register -> Register -> CodeGen ()
immRegOp SUB "0" src dest = do 
  destReg <- loadDstRegister dest
  srcReg <- loadDstRegister src
  emit $ show MOV ++ " " ++ show srcReg ++ ", " ++ show destReg
  emit $ show NEG ++ " " ++ show destReg
  storeRegister dest
immRegOp op imm src dest = do
  destReg <- loadDstRegister dest
  srcReg <- loadSrcRegister src
  emit $ show MOV ++ " " ++ show srcReg ++ ", " ++ show destReg
  emit $ show op ++ " " ++ makeImm imm ++ ", " ++ show destReg
  storeRegister dest

immOp :: Operations -> String -> String -> Register -> CodeGen ()
immOp op imm1 imm2 dest = do
  destReg <- loadDstRegister dest
  emit $ show MOV ++ " " ++ makeImm imm1 ++ ", " ++ show destReg
  emit $ show op ++ " " ++ makeImm imm2 ++ ", " ++ show destReg
  storeRegister dest

immOp' :: Operations -> String -> Register -> CodeGen ()
immOp' op imm dest = do
  destReg <- loadDstRegister dest
  emit $ show op ++ " " ++ makeImm imm ++ ", " ++ show destReg
  storeRegister dest

colorVariables :: [LiveRegisters] -> RegisterMap
colorVariables liveRegs = convertColoringToRegisterMap $ coloring graph
  where
    graph = buildgraph (map fromInteger (makeVerticesFromLive liveRegs)) (makeEdgesFromLive liveRegs)

convertColoringToRegisterMap :: [(Int, Int)] -> RegisterMap
convertColoringToRegisterMap colorPairs = convertColoringToRegisterMap' colorPairs Map.empty
  where
    convertColoringToRegisterMap' :: [(Int, Int)] -> RegisterMap -> RegisterMap
    convertColoringToRegisterMap' [] _ = Map.empty
    convertColoringToRegisterMap' ((var_name, col) : xs) m
      | col < length usableRegisters = Map.insert (fromIntegral var_name) (usableRegisters !! col) (convertColoringToRegisterMap' xs m)
      | otherwise = Map.insert (fromIntegral var_name) (Spilled (col - length usableRegisters)) (convertColoringToRegisterMap' xs m)

makeVerticesFromLive :: [LiveRegisters] -> [Register]
makeVerticesFromLive liveRegs = HS.toList (HS.fromList (makeVerticesFromLive' liveRegs))
  where
    makeVerticesFromLive' [] = []
    makeVerticesFromLive' (x : xs) = HS.toList x ++ makeVerticesFromLive' xs

makeEdgesFromLive :: [LiveRegisters] -> [Edge]
makeEdgesFromLive liveRegs = HS.toList (HS.fromList (swapToSmallestFirst (makeEdgesFromLive' liveRegs)))
  where
    makeEdgesFromLive' [] = []
    makeEdgesFromLive' (x : xs) = hashSetToPairs x ++ makeEdgesFromLive' xs

hashSetToPairs :: LiveRegisters -> [Edge]
hashSetToPairs hs = listToPairs $ map fromInteger $ HS.toList hs
  where
    listToPairs [] = []
    listToPairs (x : xs) = zip (repeat x) xs ++ listToPairs xs

swapToSmallestFirst :: (Ord a) => [(a, a)] -> [(a, a)]
swapToSmallestFirst = map normalizeOrder
  where
    normalizeOrder (a, b)
      | a <= b = (a, b)
      | otherwise = (b, a)