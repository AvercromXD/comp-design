module Compile.Liveness where

import Compile.AAAST (AAAST (..), Inst (..), Operand (..))
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as HashSet

type Register = Integer

type Line = Integer

type Use = Bool

type Def = Bool

type Live = Bool

type TagMap = Map.Map Integer (Use, Def)

type Matrix = [TagMap]

type LiveRegisters = HashSet.Set Register

type TaggedVariableLines a = State TaggedVariableLinesState a

data TaggedVariableLinesState = TaggedVariableLinesState
  { array :: Matrix,
    succs :: [[Line]],
    live :: [LiveRegisters]
  }

tagLines :: AAAST -> TaggedVariableLinesState
tagLines (Block insts) = execState (tag insts) initialState
  where
    initialState = TaggedVariableLinesState [] [] [HashSet.empty | _ <- insts]

liveness :: TaggedVariableLinesState -> [LiveRegisters]
liveness state = live $ execState (mapM_ tagLive reverse [0..length live - 1]) state

tagLive :: Line -> TaggedVariableLines ()
tagLive l = do
  liveSuccessors <- liveInSuccs l
  mapM_ (`makeLive` l)  [x | x <- liveSuccessors, y <- (isDefined x line)]


validRegistersInLine :: Line -> TaggedVariableLines [Register]
validRegistersInLine = do
  liveSuccessors <- liveInSuccs l
  return [x | x <- liveSuccessors, x ]


tag :: [Inst] -> TaggedVariableLines ()
tag insts = mapM_ (uncurry tagLine) (zip insts [0 ..])

markAsDef :: Register -> TagMap -> TagMap
markAsDef reg = Map.insert reg (False, True)

markAsUse :: Register -> TagMap -> TagMap
markAsUse reg = Map.insert reg (True, False)

markAsUseDef :: Register -> TagMap -> TagMap
markAsUseDef reg = Map.insert reg (True, True)

isInUse :: Register -> Line -> TaggedVariableLines Bool
isInUse r l = do
  a <- gets array
  let m = a !! fromIntegral l
  let t = maybe False snd $ Map.lookup r m
  return t

isDefined :: Register -> Line -> TaggedVariableLines Bool
isDefined r l = do
  a <- gets array
  let m = a !! fromIntegral l
  let t = maybe False fst $ Map.lookup r m
  return t

liveInSucc :: [Line] -> TaggedVariableLines [Register]
liveInSucc [] = []
liveInSucc (x:xs) = do
  live <- gets live
  hash <- live !! x
  return [y | member hash y, y <- registersInLine x] ++ liveInSucc xs


liveInSuccs :: Line -> TaggedVariableLines [Register]
liveInSuccs l = do
  succs <- gets succs
  let succ = succs !! l
  return liveInSucc succ

registersInLine :: Line -> TaggedVariableLines [Register]
registersInLine l = do
  a <- gets array
  let m = a !! fromIntegral l
  return HashMap.keys m


isLive :: Register -> Line -> TaggedVariableLines Bool
isLive r l = do
  lr <- gets live
  let m = lr !! fromIntegral l
  return $ HashSet.member r m

makeLive :: Register -> Line -> TaggedVariableLines ()
makeLive r l = do
  lr <- gets live
  let m = lr !! fromIntegral l
  let m2 = HashSet.insert r m
  modify $ \s -> s {live = updateAt (fromIntegral l) m2 lr}

updateAt :: Int -> a -> [a] -> [a]
updateAt _ _ [] = []
updateAt 0 newValue (_:xs) = newValue:xs
updateAt n newValue (x:xs) = x : updateAt (n-1) newValue xs

updateState :: TagMap -> Maybe Line -> TaggedVariableLines ()
updateState m (Just l) = do
  modify $ \s -> s {array = array s ++ [m]}
  modify $ \s -> s {succs = succs s ++ [[l]]}
updateState m Nothing = do
  modify $ \s -> s {array = array s ++ [m]}
  modify $ \s -> s {succs = succs s ++ [[]]}

handleRegOperand :: Integer -> Integer -> TagMap -> TagMap
handleRegOperand destReg srcReg m =
  if destReg == srcReg
    then markAsUseDef destReg m
    else markAsUse srcReg (markAsDef destReg m)

tagLine :: Inst -> Line -> TaggedVariableLines ()
tagLine (Init r1 (Reg r2)) l = do
  let m = handleRegOperand r1 r2 Map.empty
  updateState m (Just (l + 1))
tagLine (Init r (Con _)) l = do
  let m = markAsDef r Map.empty
  updateState m (Just (l + 1))
tagLine (Asgn r1 _ (Reg r2)) l = do
  let m = handleRegOperand r1 r2 Map.empty
  updateState m (Just (l + 1))
tagLine (Asgn r _ (Con _)) l = do
  let m = markAsDef r Map.empty
  updateState m (Just (l + 1))
tagLine (UnOpAsgn r _) l = do
  let m = handleRegOperand r r Map.empty
  updateState m (Just (l + 1))
tagLine (Ret (Reg r)) _ = do
  let m = markAsUse r Map.empty
  updateState m Nothing
tagLine (Ret (Con _)) _ = do
  updateState Map.empty Nothing
