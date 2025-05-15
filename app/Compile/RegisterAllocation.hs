module RegisterAllocation
  ( allocateRegisters,
  )
where

import Compile.AAAST (AAAST (Block), Inst (..), Operand (..))
import Compile.GraphColoring (Edge, buildgraph, coloring, color)
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
  | R14
  | R15 -- Reserved for spilling
  | Rsp -- Stack pointer
  | Spilled Int -- Spilled register
  deriving (Eq, Ord)

instance Show X86_64Register where
  show Rax = "%eax"
  show Rbx = "%ebx"
  show Rcx = "%ecx"
  show Rdx = "%edx"
  show Rsi = "%esi"
  show Rdi = "%edi"
  show R8 = "%r8d"
  show R9 = "%r9d"
  show R10 = "%r10d"
  show R11 = "%r11d"
  show R12 = "%r12d"
  show R13 = "%r13d"
  show R14 = "%r14d"
  show R15 = "%r15d"
  show Rsp = "%rsp"
  show (Spilled n) = error "Spilled register not supported in this context"

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
    R13,
    R14
  ]

tempReg :: X86_64Register
tempReg = R15

type RegisterMap = Map.Map Integer X86_64Register

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
  { regMap :: RegisterMap,
    code :: [String]
  }

allocateRegisters :: AAAST -> [LiveRegisters] -> [String]
allocateRegisters (Block inst) liveRegs = code $ execState (genBlock inst) initialState
  where
    initialState = CodeGenState (colorVariables liveRegs) ["subq $" ++ show (Map.size (colorVariables liveRegs) * 4) ++ ", %rsp"]

emit :: String -> CodeGen ()
emit s = modify $ \s' -> s' {code = code s' ++ [s]}

genBlock :: [Inst] -> CodeGen ()
genBlock = mapM_ genInst

lookupReg :: Register -> CodeGen X86_64Register
lookupReg reg = do
  m <- gets regMap
  case Map.lookup reg m of
    Just r -> return r
    Nothing -> error $ "Register " ++ show reg ++ " not found in register map"

-- | Retrieve the value of temp register %r15 from the stack
retrieveFromStack :: X86_64Register -> CodeGen ()
retrieveFromStack (Spilled n) = do
  emit $ "movl " ++ "[" ++ show Rsp ++ "+" ++ show n  ++ "]" ++ ", " ++ show tempReg
retrieveFromStack _ = error "Not a spilled register"

-- | Store the value of temp register %r15 to the stack
storeToStack :: X86_64Register -> CodeGen ()
storeToStack (Spilled n) = do
  emit $ "movl " ++ show tempReg ++ ", [" ++ show Rsp ++ "+" ++ show n  ++ "]"
storeToStack _ = error "Not a spilled register"

isSpilled :: X86_64Register -> Bool
isSpilled (Spilled _) = True

genInst :: Inst -> CodeGen ()
genInst (Init dest (Reg src)) = do
  srcReg <- lookupReg src
  destReg <- lookupReg dest
  emit $ "movl " ++ show srcReg ++ ", " ++ show destReg
genInst (Init dest (Con src)) = do
  destReg <- lookupReg dest
  emit $ "movl $" ++ src ++ ", " ++ show destReg

colorVariables :: [LiveRegisters] -> RegisterMap
colorVariables liveRegs = convertColoringToRegisterMap $ coloring graph
  where
    graph = buildgraph (map fromInteger (makeVerticesFromLive liveRegs)) (makeEdgesFromLive liveRegs)

convertColoringToRegisterMap :: [(Int, Int)] -> RegisterMap
convertColoringToRegisterMap colorPairs = convertColoringToRegisterMap' colorPairs Map.empty
  where
    convertColoringToRegisterMap' :: [(Int, Int)] -> RegisterMap -> RegisterMap
    convertColoringToRegisterMap' [] _ = Map.empty
    convertColoringToRegisterMap' ((var_name, color) : xs) m
      | color < length usableRegisters = Map.insert (fromIntegral var_name) (usableRegisters !! color) (convertColoringToRegisterMap' xs m)
      | otherwise = Map.insert (fromIntegral var_name) (Spilled (color - length usableRegisters)) (convertColoringToRegisterMap' xs m)

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