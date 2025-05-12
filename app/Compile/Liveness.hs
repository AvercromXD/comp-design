module Compile.Liveness where

import Compile.AAAST (AAAST (..), Inst (..), Operand (..))
import Control.Monad.State
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map

type Register = Integer

type Line = Integer

type Use = Bool

type Def = Bool

type TagMap = Map.Map Integer (Use, Def)

type Matrix = [TagMap]

type TaggedVariableLines a = State TaggedVariableLinesState a

data TaggedVariableLinesState = TaggedVariableLinesState
  { array :: Matrix,
    succs :: [[Line]]
  }

numVar :: AAAST -> Integer
numVar (Block insts) = HashSet.size HashSet.fromList (concat (map listInst insts))
  where
    listInst (Init r _) = [r]
    listInst (Asgn r1 _ (Reg r2)) = [r1, r2]
    listInst (Asgn r1 _ _) = [r1]
    listInst (UnOpAsgn r _) = [r]
    listInst (Ret (Reg r)) = [r]
    listInst (Ret (Con i)) = []

tagLines :: AAAST -> TaggedVariableLinesState
tagLines (Block insts) = execState (tag insts) initialState
  where
    initialState = TaggedVariableLinesState [] [[]]

tag :: AAAST -> TaggedVariableLines ()
tag (Block insts) = mapM_ tagLine (zip [0 ..] insts)

tagLine :: Inst -> Line -> TaggedVariableLines ()
tagLine (Init r1 (Reg r2)) l = do
  let map :: TagMap = Map.empty
  Map.insert r1 (False, True) map
  if r1 == r2 then Map.insert r1 (True, True) map else Map.insert r2 (True, False) map
  modify $ \s -> s {array = array s ++ [map]}
  modify $ \s -> s {succs = succs s ++ [l + 1]}
tagLine (Init r (Con c)) l = do
  let map :: TagMap = Map.empty
  Map.insert r (False, True) map
  modify $ \s -> s {array = array s ++ [map]}
  modify $ \s -> s {succs = succs s ++ [l + 1]}
tagLine (Asgn r1 _ (Reg r2)) l = do
  let map :: TagMap = Map.empty
  Map.insert r1 (False, True) map
  if r1 == r2 then Map.insert r1 (True, True) map else Map.insert r2 (True, False) map
  modify $ \s -> s {array = array s ++ [map]}
  modify $ \s -> s {succs = succs s ++ [l + 1]}
tagLine (Asgn r _ (Con c)) l = do
  let map :: TagMap = Map.empty
  Map.insert r (False, True) map
  modify $ \s -> s {array = array s ++ [map]}
  modify $ \s -> s {succs = succs s ++ [l + 1]}
tagLine (UnOpAsgn r _) l = do
  let map :: TagMap = Map.empty
  Map.insert r (False, True) map
  modify $ \s -> s {array = array s ++ [map]}
  modify $ \s -> s {succs = succs s ++ [l + 1]}
tagLine (Ret (Reg r)) l = do
  let map :: TagMap = Map.empty
  Map.insert r (True, False) map
  modify $ \s -> s {array = array s ++ [map]}
  modify $ \s -> s {succs = succs s ++ []}
tagLine (Ret (Con c)) l = do
  let map = Map.empty
  modify $ \s -> s {array = array s ++ [map]}
  modify $ \s -> s {succs = succs s ++ []}
