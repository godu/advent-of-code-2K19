module Day08Spec where

import Day08
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "part A" $ checkSum (3, 2) "123456789012" `shouldBe` 1
  it "part B" $
    renderVisibleImage (2, 2) "0222112222120000" `shouldBe` [0, 1, 1, 0]
