module Day09Spec where

import Day09
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "part A" $ do
    runA [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] [] `shouldBe` [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
    runA [1102,34915192,34915192,7,4,7,99,0] [] `shouldBe` [1219070632396864]
    runA [104,1125899906842624,99] [] `shouldBe` [1125899906842624]