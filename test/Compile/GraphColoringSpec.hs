module Compile.GraphColoringSpec where

import Compile.GraphColoring
import Test.Hspec

spec :: Spec
spec = do
  describe "Graph Coloring" $ do
    do
      "buildGraph" $ do
        it "should build a graph from vertices and edges" $ do
          let vertices = [0, 1, 2]
          let edges = [(0, 1), (1, 2)]
          let expectedGraph = Map.fromList [(0, [1]), (1, [0, 2]), (2, [1])]
          buildgraph vertices edges `shouldBe` expectedGraph