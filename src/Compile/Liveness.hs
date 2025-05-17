{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Compile.Liveness where

import Compile.AAAST (AAAST (..), Inst (..), Operand (..))
import Control.Monad.State (State, execState, gets, modify)
import qualified Data.Map as Map
import qualified Data.HashSet as HashSet
import Control.Monad (filterM)

type Register = Integer

type Line = Integer

type Use = Bool

type Def = Bool

type Live = Bool

type TagMap = Map.Map Integer (Use, Def)

type Matrix = [TagMap]

type LiveRegisters = HashSet.HashSet Register

type TaggedVariableLines a = State TaggedVariableLinesState a

data TaggedVariableLinesState = TaggedVariableLinesState
  { array :: Matrix,
    succs :: [[Line]],
    live :: [LiveRegisters]
  }

instance Show TaggedVariableLinesState where
  show :: TaggedVariableLinesState -> String
  show (TaggedVariableLinesState a s l) =
    "TaggedVariableLinesState { \n array = " ++ show a ++ "\n succs = " ++ show s ++ "\n live = " ++ show l ++ " } \n"

tagLines :: AAAST -> TaggedVariableLinesState
tagLines (Block insts) = execState (tag insts) initialState
  where
    initialState = TaggedVariableLinesState [] [] [HashSet.empty | _ <- insts]

liveness :: TaggedVariableLinesState -> [LiveRegisters]
liveness state = live $ execState liveness' state
  where
    liveness' = do
      liveList <- gets live
      mapM_ (tagLive . fromIntegral) (reverse [0 .. length liveList - 1])
      mapM_ (tagDefLive . fromIntegral) [0.. length liveList - 1]

tagDefLive :: Line -> TaggedVariableLines ()
tagDefLive l = do 
  regs <- registersInLine l
  def <- filterM (`isDefined` l) regs
  mapM_(`makeLive` l) def

tagLive :: Line -> TaggedVariableLines ()
tagLive l = do
  liveSuccessors <- liveInSuccs l
  defined <- filterM (\s -> fmap not (isDefined s l)) (HashSet.toList liveSuccessors)
  mapM_ (`makeLive` l) defined
  regs <- registersInLine l
  used <- filterM (`isInUse` l) regs
  mapM_ (`makeLive` l) used


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
  let t = maybe False fst $ Map.lookup r m
  return t

isDefined :: Register -> Line -> TaggedVariableLines Bool
isDefined r l = do
  a <- gets array
  let m = a !! fromIntegral l
  let t = maybe False snd $ Map.lookup r m
  return t

liveInSucc :: [Line] -> TaggedVariableLines [Register]
liveInSucc [] = return []
liveInSucc (x:xs) = do
  liveR <- gets live
  let hash = liveR !! fromIntegral x
  rest <- liveInSucc xs
  return (HashSet.toList hash ++ rest)


liveInSuccs :: Line -> TaggedVariableLines (HashSet.HashSet Register)
liveInSuccs l = do
  succs' <- gets succs
  let succ' = succs' !! fromIntegral l
  liveSuccs <- liveInSucc succ'
  return (HashSet.fromList liveSuccs)

registersInLine :: Line -> TaggedVariableLines [Register]
registersInLine l = do
  a <- gets array
  let m = a !! fromIntegral l
  return (Map.keys m)


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
  len <- gets (length . live)
  if l >= fromIntegral len then do
    modify $ \s -> s {succs = succs s ++ [[]]}
  else do 
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
tagLine (Asgn r (Reg r1) _ (Reg r2)) l = do
  let m = markAsUse r2 Map.empty
  let m1 = markAsUse r1 m
  let m2 = if r == r1 || r == r2 then markAsUseDef r m1 else markAsDef r m1
  updateState m2 (Just (l + 1))
tagLine (Asgn r (Reg r1) _ (Con _)) l = do
  let m = markAsUse r1 Map.empty
  let m1 = if r == r1 then do markAsUseDef r m else markAsDef r m
  updateState m1 (Just (l + 1))
tagLine (Asgn r (Con _) _ (Reg r1)) l = do
  let m = markAsUse r1 Map.empty
  let m1 = if r == r1 then markAsUseDef r m else markAsDef r m
  updateState m1 (Just (l + 1))
tagLine (Asgn r (Con _) _ (Con _)) l = do
  let m = markAsDef r Map.empty
  updateState m (Just (l + 1))
tagLine (Ret (Reg r)) _ = do
  let m = markAsUse r Map.empty
  updateState m Nothing
tagLine (Ret (Con _)) _ = do
  updateState Map.empty Nothing
