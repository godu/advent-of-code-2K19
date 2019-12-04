module Day04Spec where

import Data.List
import Day04
  ( hasDigitsOnlyIncrease
  , hasSameTwoAdjacentDigits
  , isASixDigitNumber
  , partA
  , partB
  , toDigits
  )
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "isASixDigitNumber" $ do
    isASixDigitNumber 12345 `shouldBe` False
    isASixDigitNumber 123456 `shouldBe` True
    isASixDigitNumber 1234567 `shouldBe` False
  it "toDigits" $ do
    toDigits 12345 `shouldBe` [5, 4, 3, 2, 1]
    toDigits 123456 `shouldBe` [6, 5, 4, 3, 2, 1]
    toDigits 1234567 `shouldBe` [7, 6, 5, 4, 3, 2, 1]
  it "hasSameTwoAdjacentDigits" $ do
    hasSameTwoAdjacentDigits 123456 `shouldBe` False
    hasSameTwoAdjacentDigits 123445 `shouldBe` True
    hasSameTwoAdjacentDigits 122445 `shouldBe` True
    hasSameTwoAdjacentDigits 124445 `shouldBe` True
    hasSameTwoAdjacentDigits 111111 `shouldBe` True
  it "hasDigitsOnlyIncrease" $ do
    hasDigitsOnlyIncrease 111123 `shouldBe` True
    hasDigitsOnlyIncrease 135679 `shouldBe` True
    hasDigitsOnlyIncrease 123456 `shouldBe` True
    hasDigitsOnlyIncrease 112233 `shouldBe` True
    hasDigitsOnlyIncrease 113355 `shouldBe` True
    hasDigitsOnlyIncrease 111111 `shouldBe` True
    hasDigitsOnlyIncrease 123454 `shouldBe` False
    hasDigitsOnlyIncrease 654321 `shouldBe` False
    hasDigitsOnlyIncrease 612345 `shouldBe` False
  it "part A" $ do
    partA "111111-111111" `shouldBe` 1
    partA "223450-223450" `shouldBe` 0
    partA "123789-123789" `shouldBe` 0
  it "part B" $ do
    partB "112233-112233" `shouldBe` 1
    partB "123444-123444" `shouldBe` 0
    partB "111122-111122" `shouldBe` 1
