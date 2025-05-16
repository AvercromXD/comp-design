module Compile
  ( Job (..),
    compile,
  )
where

import Compile.InstructionSelection (codeGen)
import Compile.Liveness (liveness, tagLines)
import Compile.Parser (parseAST)
import Compile.RegisterAllocation (allocateRegisters)
import Compile.Semantic (semanticAnalysis)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (intercalate)
import Error (L1ExceptT)

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
  -- liftIO $ appendFile (out job) "\n"
  -- liftIO $ appendFile (out job) (show live)
  starterCode <- liftIO $ readFile "res/starter_code"
  liftIO $ writeFile (out job) starterCode
  liftIO $ appendFile (out job) "\n"
  liftIO $ appendFile (out job) (intercalate "\n" output)
  liftIO $ appendFile (out job) "\n"

  return ()
