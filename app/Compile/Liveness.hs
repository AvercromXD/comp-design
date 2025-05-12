module Compile.Liveness where

import Compile.AAAST (AAAST (..), Inst (..), Operand (..))

type Register = Integer

type Line = Integer

data Tag
  = Def Register
  | Use Register
  | Succ Line
  deriving Eq


type Live = Register

type TaggedLine = Line Succs [[Tag]]
type Succs = [Tag]

tagInsts :: AAAST -> [(Line, [[Tag]])]
tagInsts (Block insts) = map (\(line, inst) -> (line, tagInst inst line)) (zip [0 ..] insts)

tagInst :: Inst -> Line -> [Tag]
tagInst (Init r (Reg i)) l = [Def r, Use i, Succ l + 1]
tagInst (Init r (Con c)) l = [Def r, Succ l + 1]
tagInst (Ret (Reg i)) l = [Use i]
tagInst (Ret (Con c)) l = []
tagInst (Asgn r _ (Con c)) l = [Def r, Use r, Succ l + 1]
tagInst (Asgn r _ (Reg i)) l = [Def r, Use r, Use i, Succ l + 1]

tagLiveInsts :: [(Line, [Tag])] -> [(Line, [Live])]

tagLive :: [(Line, [Tag])] -> Line -> [Tag] -> [Live]
tagLive lines line tags
    |


hasTag :: [(Line, [Tag])] -> Line -> Tag -> Boolean
hasTag [] line tag = False
hasTag ((line, tags) : xs) line tag = elem tag tags
hasTag (x : xs) line tag = hasTag xs line tag
