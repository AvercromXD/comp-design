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
  semanticAnalysis ast
  let code = codeGen ast
  let taggedLines = tagLines code
  let live = liveness taggedLines
  let output = allocateRegisters code live
  starterCode <- liftIO $ readFile "res/starter_code"
  liftIO $ writeFile (out job) starterCode
  liftIO $ print ast
  liftIO $ print code
  liftIO $ print live
  liftIO $ appendFile (out job) "\n"
  liftIO $ appendFile (out job) (intercalate "\n" output)
  liftIO $ appendFile (out job) "\n"

  return ()
