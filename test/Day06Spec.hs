module Day06Spec where

import Day06 (partA, partB)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "part A" $ partA "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"  `shouldBe` 42
  it "part B" $ partB "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN" `shouldBe` 4
