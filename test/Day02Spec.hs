module Day02Spec where

import Day02 (partA, partB, runProgram)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "runProgram" $ do
    runProgram [1, 0, 0, 0, 99] `shouldBe` [2, 0, 0, 0, 99]
    runProgram [2, 3, 0, 3, 99] `shouldBe` [2, 3, 0, 6, 99]
    runProgram [2, 4, 4, 5, 99, 0] `shouldBe` [2, 4, 4, 5, 99, 9801]
    runProgram [1, 1, 1, 4, 99, 5, 6, 0, 99] `shouldBe`
      [30, 1, 1, 4, 2, 5, 6, 0, 99]
  it "part A" $ partA "1,9,10,3,2,3,11,0,99,30,40,50,0" `shouldBe` 100
