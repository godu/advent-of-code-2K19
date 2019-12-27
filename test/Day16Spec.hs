module Day16Spec where

import Day16
import Test.Hspec

spec :: Spec
spec = do
  -- it "basePattern" $ do
  --   take 8 (basePattern 1) `shouldBe` [1, 0, -1, 0, 1, 0, -1, 0]
  --   take 8 (basePattern 2) `shouldBe` [0, 1, 1, 0, 0, -1, -1, 0]
  --   take 8 (basePattern 3) `shouldBe` [0, 0, 1, 1, 1, 0, 0, 0]
  it "phase" $ do
    phase [1, 2, 3, 4, 5, 6, 7, 8] `shouldBe` [4, 8, 2, 2, 6, 1, 5, 8]
    phase [4, 8, 2, 2, 6, 1, 5, 8] `shouldBe` [3, 4, 0, 4, 0, 4, 3, 8]
    phase [3, 4, 0, 4, 0, 4, 3, 8] `shouldBe` [0, 3, 4, 1, 5, 5, 1, 8]
  it "part A" $ do
    partA "80871224585914546619083218645595" `shouldBe` 24176176
    partA "19617804207202209144916044189917" `shouldBe` 73745418
    partA "69317163492948606335995924319873" `shouldBe` 52432133
  -- it "part B" $ do
  --   partB "80871224585914546619083218645595" `shouldBe` 84462026
  --   partB "19617804207202209144916044189917" `shouldBe` 78725270
  --   partB "69317163492948606335995924319873" `shouldBe` 53553731
