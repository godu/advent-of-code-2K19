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

getPrograms :: [Int] -> [Int] -> [Program]
getPrograms memory inputs = runProgram $ toProgram memory inputs

spec :: Spec
spec = do
  it "runProgram#addition" $ do
    memory (last (getPrograms [1, 0, 0, 0, 99] [])) `shouldBe` [2, 0, 0, 0, 99]
    memory (last (getPrograms [1, 1, 1, 4, 99, 5, 6, 0, 99] [])) `shouldBe`
      [30, 1, 1, 4, 2, 5, 6, 0, 99]
    memory (last (getPrograms [1, 0, 0, 0, 99] [])) `shouldBe` [2, 0, 0, 0, 99]
    memory (last (getPrograms [101, 3, 0, 0, 99] [])) `shouldBe`
      [104, 3, 0, 0, 99]
  it "runProgram#multiplication" $ do
    memory (last (getPrograms [2, 3, 0, 3, 99] [])) `shouldBe` [2, 3, 0, 6, 99]
    memory (last (getPrograms [2, 4, 4, 5, 99, 0] [])) `shouldBe`
      [2, 4, 4, 5, 99, 9801]
  it "runProgram#outputs" $ do
    mapMaybe output (getPrograms [1, 0, 0, 0, 4, 0, 99] []) `shouldBe` [2]
    mapMaybe output (getPrograms [101, 3, 0, 0, 4, 0, 99] []) `shouldBe` [104]
  it "runProgram#inputs" $ do
    inputs (last (getPrograms [3, 0, 99] [42])) `shouldBe` []
    memory (last (getPrograms [3, 0, 99] [42])) `shouldBe` [42, 0, 99]
  it "runProgram#equals" $ do
    memory (last (getPrograms [8, 5, 6, 5, 99, 7, 8] [])) `shouldBe`
      [8, 5, 6, 5, 99, 0, 8]
    memory (last (getPrograms [8, 5, 6, 5, 99, 8, 8] [])) `shouldBe`
      [8, 5, 6, 5, 99, 1, 8]
    memory (last (getPrograms [8, 5, 6, 5, 99, 9, 8] [])) `shouldBe`
      [8, 5, 6, 5, 99, 0, 8]
    memory (last (getPrograms [1108, 7, 8, 1, 99] [])) `shouldBe`
      [1108, 0, 8, 1, 99]
    memory (last (getPrograms [1108, 8, 8, 1, 99] [])) `shouldBe`
      [1108, 1, 8, 1, 99]
    memory (last (getPrograms [1108, 9, 8, 1, 99] [])) `shouldBe`
      [1108, 0, 8, 1, 99]
  it "runProgram#less" $ do
    memory (last (getPrograms [7, 5, 6, 5, 99, 7, 8] [])) `shouldBe`
      [7, 5, 6, 5, 99, 1, 8]
    memory (last (getPrograms [7, 5, 6, 5, 99, 8, 8] [])) `shouldBe`
      [7, 5, 6, 5, 99, 0, 8]
    memory (last (getPrograms [7, 5, 6, 5, 99, 9, 8] [])) `shouldBe`
      [7, 5, 6, 5, 99, 0, 8]
    memory (last (getPrograms [1107, 7, 8, 1, 99] [])) `shouldBe`
      [1107, 1, 8, 1, 99]
    memory (last (getPrograms [1107, 8, 8, 1, 99] [])) `shouldBe`
      [1107, 0, 8, 1, 99]
    memory (last (getPrograms [1107, 9, 8, 1, 99] [])) `shouldBe`
      [1107, 0, 8, 1, 99]
  it "runProgram#jumpIfTrue" $ do
    mapMaybe output (getPrograms [1105, 0, 5, 104, 42, 99] []) `shouldBe` [42]
    mapMaybe output (getPrograms [1105, 1, 5, 104, 42, 99] []) `shouldBe` []
    mapMaybe output (getPrograms [5, 6, 7, 104, 42, 99, 0, 5] []) `shouldBe`
      [42]
    mapMaybe output (getPrograms [5, 6, 7, 104, 42, 99, 1, 5] []) `shouldBe` []
  it "runProgram#jumpIfFalse" $ do
    mapMaybe output (getPrograms [1106, 0, 5, 104, 42, 99] []) `shouldBe` []
    mapMaybe output (getPrograms [1106, 1, 5, 104, 42, 99] []) `shouldBe` [42]
    mapMaybe output (getPrograms [6, 6, 7, 104, 42, 99, 0, 5] []) `shouldBe` []
    mapMaybe output (getPrograms [6, 6, 7, 104, 42, 99, 1, 5] []) `shouldBe`
      [42]
  it "runProgram" $ do
    mapMaybe output (getPrograms largeInput [7]) `shouldBe` [999]
    mapMaybe output (getPrograms largeInput [8]) `shouldBe` [1000]
    mapMaybe output (getPrograms largeInput [9]) `shouldBe` [1001]
