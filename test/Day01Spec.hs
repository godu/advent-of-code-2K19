module Day01Spec where

import Day01 (partA, partB)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "part A" $ do
    partA "12" `shouldBe` 2
    partA "14" `shouldBe` 2
    partA "1969" `shouldBe` 654
    partA "100756" `shouldBe` 33583
  it "part B" $ do
    partB "14" `shouldBe` 2
    partB "1969" `shouldBe` 966
    partB "100756" `shouldBe` 50346
