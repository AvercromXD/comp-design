module Compile
  ( Job (..),
    compile,
  )
where

import Compile.AAsm (codeGen)
import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Control.Monad.IO.Class
import Error (L1ExceptT)

data Job = Job
  { src :: FilePath,
    out :: FilePath
  }
  deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
  ast <- parseAST $ src job
  liftIO $ writeFile (out job) (show ast)
  liftIO $ appendFile (out job) "\n"
  semanticAnalysis ast
  let code = codeGen ast
  liftIO $ appendFile (out job) (show code)
  return ()
