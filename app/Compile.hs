module Compile
  ( Job (..),
    compile,
  )
where

import Compile.InstructionSelection (codeGen)
import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Compile.Liveness (tagLines, liveness)
import Compile.RegisterAllocation(allocateRegisters)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Error (L1ExceptT)
import Data.List (intercalate)


data Job = Job
  { src :: FilePath,
    out :: FilePath
  }
  deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
  ast <- parseAST $ src job
  -- liftIO $ writeFile (out job) (show ast)
  -- liftIO $ appendFile (out job) "\n"
  semanticAnalysis ast
  let code = codeGen ast
  -- liftIO $ appendFile (out job) (show code)
  let taggedLines = tagLines code
  -- liftIO $ appendFile (out job) "\n"
  -- liftIO $ appendFile (out job) (show taggedLines)
  let live = liveness taggedLines
  let output = allocateRegisters code live
  starterCode <- liftIO $ readFile "res/starter_code"
  liftIO $ writeFile (out job) starterCode
  liftIO $ appendFile (out job) intercalate "\n" (map show output)
  -- liftIO $ appendFile (out job) "\n"
  -- liftIO $ appendFile (out job) (show live)
  return ()
