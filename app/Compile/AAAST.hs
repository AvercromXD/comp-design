-- | This module provides a data type for the abstract assembly
-- representation of the abstract syntax tree used for instruction selection
module Compile.AAAST
    (
        AAAST(..)
        , Inst(..)
        , Operand(..)
    ) where

import Compile.AST (Op)
import Data.List (intercalate)

type Register = Integer

data AAAST =
    Block [Inst]

data Inst
    = Init Register Operand -- d <- s
    | Asgn Register Op Operand -- d <- d Op s
    | UnOpAsgn Register Op -- d <- Op d
    | Ret Operand

data Operand
    = Reg Register
    | Con Integer


instance Show AAAST where
    show (Block insts) =
        "Block: {\n" ++ intercalate "\n" (map show insts) ++ "\n}"

instance Show Inst where
    show (Init dest op) = "Init: " ++ "R" ++ show dest ++ " <- " ++ show op
    show (Asgn dest op op1) = "Assign: " ++ "R" ++ show dest ++ " <- " ++ "R" ++ show dest ++ " " ++ show op ++ " " ++ show op1
    show (UnOpAsgn dest op) = "UnOpAssign" ++ "R" ++ show dest ++ " <- " ++ show op ++ " " ++ show op
    show (Ret op1) = "Return " ++ show op1

instance Show Operand where
    show (Reg i) = 'R' : show i
    show (Con c) = show c