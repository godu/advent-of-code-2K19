module Day05Spec where

import Day05 (partA, partB, runProgram)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "runProgram" $ do
    runProgram [1, 0, 0, 0, 4, 0, 99] [] `shouldBe` [2]
    runProgram [101, 3, 0, 0, 4, 0, 99] [] `shouldBe` [104]
    runProgram [2, 3, 0, 3, 4, 3, 99] [] `shouldBe` [6]
    runProgram [3, 0, 99] [42] `shouldBe` []
    runProgram [4, 0, 99] [42] `shouldBe` [4]
    runProgram [3, 0, 4, 0, 99] [42] `shouldBe` [42]
    runProgram [1101, 100, -2, 7, 4, 7, 99, 0] [42] `shouldBe` [98]
    runProgram
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
      [7] `shouldBe`
      [999]
    runProgram
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
      [8] `shouldBe`
      [1000]
    runProgram
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
      [9] `shouldBe`
      [1001]
  it "runProgram#equals" $ do
    runProgram [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] [7] `shouldBe` [0]
    runProgram [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] [8] `shouldBe` [1]
    runProgram [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] [9] `shouldBe` [0]
    runProgram [3, 3, 1108, -1, 8, 3, 4, 3, 99] [7] `shouldBe` [0]
    runProgram [3, 3, 1108, -1, 8, 3, 4, 3, 99] [8] `shouldBe` [1]
    runProgram [3, 3, 1108, -1, 8, 3, 4, 3, 99] [9] `shouldBe` [0]
  it "runProgram#less" $ do
    runProgram [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8] [7] `shouldBe` [1]
    runProgram [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8] [8] `shouldBe` [0]
    runProgram [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8] [9] `shouldBe` [0]
    runProgram [3, 3, 1107, -1, 8, 3, 4, 3, 99] [7] `shouldBe` [1]
    runProgram [3, 3, 1107, -1, 8, 3, 4, 3, 99] [8] `shouldBe` [0]
    runProgram [3, 3, 1107, -1, 8, 3, 4, 3, 99] [9] `shouldBe` [0]
  it "runProgram#jump" $ do
    runProgram [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] [2] `shouldBe`
      [1]
    runProgram [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] [0] `shouldBe`
      [0]
    runProgram [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] [2] `shouldBe`
      [1]
    runProgram [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] [0] `shouldBe`
      [0]
