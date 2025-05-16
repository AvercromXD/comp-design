module Main where

import qualified Spec
import Test.Hspec.Formatters
import Test.Hspec.Runner

main :: IO ()
main = do
  hspecWith
    Spec.specs
    defaultConfig
      { configFormat = Just (Spec.specFormatter, Spec.specOutput),
        configColorMode = ColorAuto
      }
  return ()