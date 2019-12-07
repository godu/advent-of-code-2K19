module Day05Spec where

import Data.Maybe
import Day05
import Test.Hspec (Spec, it, shouldBe)

largeInput :: [Int]
largeInput =
  [ 3
  , 21
  , 1008
  , 21
  , 8
  , 20
  , 1005
  , 20
  , 22
  , 107
  , 8
  , 21
  , 20
  , 1006
  , 20
  , 31
  , 1106
  , 0
  , 36
  , 98
  , 0
  , 0
  , 1002
  , 21
  , 125
  , 20
  , 4
  , 20
  , 1105
  , 1
  , 46
  , 104
  , 999
  , 1105
  , 1
  , 46
  , 1101
  , 1000
  , 1
  , 20
  , 4
  , 20
  , 1105
  , 1
  , 46
  , 98
  , 99
  ]

getLastProgram :: [Int] -> [Int] -> Program
getLastProgram memory inputs = last $ runProgram $ toProgram memory inputs

spec :: Spec
spec = do
  it "runProgram#addition" $ do
    memory (getLastProgram [1, 0, 0, 0, 99] []) `shouldBe` [2, 0, 0, 0, 99]
    memory (getLastProgram [1, 1, 1, 4, 99, 5, 6, 0, 99] []) `shouldBe`
      [30, 1, 1, 4, 2, 5, 6, 0, 99]
    memory (getLastProgram [1, 0, 0, 0, 99] []) `shouldBe` [2, 0, 0, 0, 99]
    memory (getLastProgram [101, 3, 0, 0, 99] []) `shouldBe` [104, 3, 0, 0, 99]
  it "runProgram#multiplication" $ do
    memory (getLastProgram [2, 3, 0, 3, 99] []) `shouldBe` [2, 3, 0, 6, 99]
    memory (getLastProgram [2, 4, 4, 5, 99, 0] []) `shouldBe`
      [2, 4, 4, 5, 99, 9801]
  it "runProgram#outputs" $ do
    outputs (getLastProgram [1, 0, 0, 0, 4, 0, 99] []) `shouldBe` [2]
    outputs (getLastProgram [101, 3, 0, 0, 4, 0, 99] []) `shouldBe` [104]
  it "runProgram#inputs" $ do
    inputs (getLastProgram [3, 0, 99] [42]) `shouldBe` []
    memory (getLastProgram [3, 0, 99] [42]) `shouldBe` [42, 0, 99]
  it "runProgram#equals" $ do
    memory (getLastProgram [8, 5, 6, 5, 99, 7, 8] []) `shouldBe`
      [8, 5, 6, 5, 99, 0, 8]
    memory (getLastProgram [8, 5, 6, 5, 99, 8, 8] []) `shouldBe`
      [8, 5, 6, 5, 99, 1, 8]
    memory (getLastProgram [8, 5, 6, 5, 99, 9, 8] []) `shouldBe`
      [8, 5, 6, 5, 99, 0, 8]
    memory (getLastProgram [1108, 7, 8, 1, 99] []) `shouldBe`
      [1108, 0, 8, 1, 99]
    memory (getLastProgram [1108, 8, 8, 1, 99] []) `shouldBe`
      [1108, 1, 8, 1, 99]
    memory (getLastProgram [1108, 9, 8, 1, 99] []) `shouldBe`
      [1108, 0, 8, 1, 99]
  it "runProgram#less" $ do
    memory (getLastProgram [7, 5, 6, 5, 99, 7, 8] []) `shouldBe`
      [7, 5, 6, 5, 99, 1, 8]
    memory (getLastProgram [7, 5, 6, 5, 99, 8, 8] []) `shouldBe`
      [7, 5, 6, 5, 99, 0, 8]
    memory (getLastProgram [7, 5, 6, 5, 99, 9, 8] []) `shouldBe`
      [7, 5, 6, 5, 99, 0, 8]
    memory (getLastProgram [1107, 7, 8, 1, 99] []) `shouldBe`
      [1107, 1, 8, 1, 99]
    memory (getLastProgram [1107, 8, 8, 1, 99] []) `shouldBe`
      [1107, 0, 8, 1, 99]
    memory (getLastProgram [1107, 9, 8, 1, 99] []) `shouldBe`
      [1107, 0, 8, 1, 99]
  it "runProgram#jumpIfTrue" $ do
    outputs (getLastProgram [1105, 0, 5, 104, 42, 99] []) `shouldBe` [42]
    outputs (getLastProgram [1105, 1, 5, 104, 42, 99] []) `shouldBe` []
    outputs (getLastProgram [5, 6, 7, 104, 42, 99, 0, 5] []) `shouldBe` [42]
    outputs (getLastProgram [5, 6, 7, 104, 42, 99, 1, 5] []) `shouldBe` []
  it "runProgram#jumpIfFalse" $ do
    outputs (getLastProgram [1106, 0, 5, 104, 42, 99] []) `shouldBe` []
    outputs (getLastProgram [1106, 1, 5, 104, 42, 99] []) `shouldBe` [42]
    outputs (getLastProgram [6, 6, 7, 104, 42, 99, 0, 5] []) `shouldBe` []
    outputs (getLastProgram [6, 6, 7, 104, 42, 99, 1, 5] []) `shouldBe` [42]
  it "runProgram" $ do
    outputs (getLastProgram largeInput [7]) `shouldBe` [999]
    outputs (getLastProgram largeInput [8]) `shouldBe` [1000]
    outputs (getLastProgram largeInput [9]) `shouldBe` [1001]
