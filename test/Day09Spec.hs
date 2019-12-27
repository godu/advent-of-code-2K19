module Day09Spec where

import Day05 (update)
import Day09
import Test.Hspec (Spec, it, shouldBe)
import qualified Data.Vector as Vector

spec :: Spec
spec = do
  it "update" $ do
    update 0 1 (Vector.fromList [0]) `shouldBe` Vector.fromList [1]
    update 1 1 (Vector.fromList [0]) `shouldBe` Vector.fromList [0, 1]
    update 2 1 (Vector.fromList [0]) `shouldBe` Vector.fromList [0, 0, 1]
  it "part A" $ do
    runA [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] [] `shouldBe` [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
    runA [1102,34915192,34915192,7,4,7,99,0] [] `shouldBe` [1219070632396864]
    runA [104,1125899906842624,99] [] `shouldBe` [1125899906842624]
  it "part B" $ do
    partB "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" `shouldBe` 3946
    partB "1102,34915192,34915192,7,4,7,99,0" `shouldBe` 1219070632396864
    partB "104,1125899906842624,99" `shouldBe` 1125899906842624