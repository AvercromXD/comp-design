module Compile.GraphColoringSpec (spec) where

import Compile.GraphColoring (buildgraph, coloring)
import qualified Data.Map as Map
import Test.Hspec

spec :: Spec
spec = do
  describe "Graph Coloring" $ do
    it "should build a graph from vertices and edges" $ do
      let vertices = [0, 1, 2]
      let edges = [(0, 1), (1, 2)]
      let expectedGraph = Map.fromList [(0, [1]), (1, [2, 0]), (2, [1])]
      buildgraph vertices edges `shouldBe` expectedGraph
      let vertices2 = [0, 1, 2, 3]
      let edges2 = [(0, 1), (1, 0), (1, 2), (2, 3)]
      let expectedGraph2 = Map.fromList [(0, [1]), (1, [0, 2]), (2, [3, 1]), (3, [2])]
      buildgraph vertices2 edges2 `shouldBe` expectedGraph2
    it "should color a graph" $ do
      let graph = Map.fromList [(0, [1]), (1, [0, 2]), (2, [1])]
      let expectedColors = [(0, 0), (1, 1), (2, 0)]
      coloring graph `shouldBe` expectedColors
      let graph2 = Map.fromList [(0, [1]), (1, [0, 2]), (2, [1, 3]), (3, [2])]
      let expectedColors2 = [(0, 0), (1, 1), (2, 0), (3, 1)]
      coloring graph2 `shouldBe` expectedColors2
      let chordalGraph = Map.fromList [(0, [1, 2, 3]), (1, [0, 2, 3]), (2, [0, 1, 3]), (3, [0, 1, 2])]
      let expectedChordalColors = [(0, 0), (1, 3), (2, 2), (3, 1)]
      coloring chordalGraph `shouldBe` expectedChordalColors
      let chordalGraph2 = Map.fromList [(0, [1, 2]), (1, [0, 2]), (2, [0, 1]), (3, [4]), (4, [3])]
      let expectedChordalColors2 = [(0, 0), (1, 2), (2, 1), (3, 0), (4, 1)]
      coloring chordalGraph2 `shouldBe` expectedChordalColors2
      let nonChordalGraph = Map.fromList [(0, [1, 3, 4]), (1, [0, 2, 4]), (2, [1, 3, 4]), (3, [0, 2, 4]), (4, [0, 1, 2, 3])]
      let expectedNonChordalColors = [(0, 0), (1, 2), (2, 0), (3, 2), (4, 1)]
      coloring nonChordalGraph `shouldBe` expectedNonChordalColors
      let noEdgesGraph = Map.fromList [(0, []), (1, []), (2, []), (3, []), (4, [])]
      let expectedNoEdgesColors = [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0)]
      coloring noEdgesGraph `shouldBe` expectedNoEdgesColors
