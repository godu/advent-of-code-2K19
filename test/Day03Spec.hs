module Day03Spec where

import Day03 (partA, partB, readInstruction, toManhattanDistance, unfoldWire)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "toManhattanDistance" $ do
    toManhattanDistance (1, 2) `shouldBe` 3
    toManhattanDistance (1, -2) `shouldBe` 3
    toManhattanDistance (-1, 2) `shouldBe` 3
    toManhattanDistance (-1, -2) `shouldBe` 3
  it "unfoldWire" $ do
    unfoldWire [(0, 0)] `shouldBe` []
    unfoldWire [(0, 2)] `shouldBe` [(0, 1), (0, 2)]
    unfoldWire [(0, -2)] `shouldBe` [(0, -1), (0, -2)]
    unfoldWire [(2, 0)] `shouldBe` [(1, 0), (2, 0)]
    unfoldWire [(-2, 0)] `shouldBe` [(-1, 0), (-2, 0)]
    unfoldWire [(0, 1), (1, 0), (0, -2), (-2, 0)] `shouldBe`
      [(0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]
  it "readInstruction" $ do
    readInstruction "U16" `shouldBe` (0, 16)
    readInstruction "D12" `shouldBe` (0, -12)
    readInstruction "L10" `shouldBe` (10, 0)
    readInstruction "R15" `shouldBe` (-15, 0)
  it "part A" $ partA "R8,U5,L5,D3\nU7,R6,D4,L4" `shouldBe` 6
  it "part B" $ partB "R8,U5,L5,D3\nU7,R6,D4,L4" `shouldBe` 30
