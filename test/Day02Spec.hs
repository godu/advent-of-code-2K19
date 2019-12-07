module Day02Spec where

import Day02 (partA, partB)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = it "part A" $ partA "1,9,10,3,2,3,11,0,99,30,40,50,0" `shouldBe` 100
