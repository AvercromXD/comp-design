module Compile.RegisterAllocationSpec (spec) where

import Compile.RegisterAllocation (colorVariables, usableFromIdx)
import qualified Data.HashSet as HS
import qualified Data.Map as Map
import Test.Hspec

spec :: Spec
spec = do
  describe "Register Allocation" $ do
    it "Should color variables correctly" $ do
      let liveRegistersList = Prelude.map HS.fromList [[0, 1, 4], [1, 2, 4], [2, 3, 4], [0, 3, 4]]
      let expectedColoredRegisters = Map.fromList $ map (\(x, y) -> (x, usableFromIdx y)) [(0, 0), (1, 2), (2, 0), (3, 2), (4, 1)]
      colorVariables liveRegistersList `shouldBe` expectedColoredRegisters