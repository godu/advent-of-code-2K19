module Day12Spec where

import Day12
import Test.Hspec
import Text.Regex.TDFA

spec :: Spec
spec = do
  it "Moon#read" $ do
    read "<x=-1, y=0, z=2>" `shouldBe` Moon (-1, 0, 2) (0, 0, 0)
    read "<x=2, y=-10, z=-7>" `shouldBe` Moon (2, -10, -7) (0, 0, 0)
    read "<x=4, y=-8, z=8>" `shouldBe` Moon (4, -8, 8) (0, 0, 0)
    read "<x=3, y=5, z=-1>" `shouldBe` Moon (3, 5, -1) (0, 0, 0)
  it "run" $ do
    (!! 0) (run [
        Moon (-7,-8,9) (0,0,0),
        Moon (4,-10,-6) (0,0,0)
      ]) `shouldBe` [
          Moon (-6,-9, 8) ( 1,-1,-1),
          Moon ( 3,-9,-5) (-1, 1, 1)
        ]
    (!! 0) (run [
        Moon (-1,  0, 2) (0,0,0),
        Moon ( 2,-10,-7) (0,0,0),
        Moon ( 4, -8, 8) (0,0,0),
        Moon ( 3,  5,-1) (0,0,0)
      ]) `shouldBe` [
          Moon ( 2, -1, 1) ( 3,-1,-1),
          Moon ( 3, -7,-4) ( 1, 3, 3),
          Moon ( 1, -7, 5) (-3, 1,-3),
          Moon ( 2,  2, 0) (-1,-3, 1)
        ]
    (!! 9) (run $ parse "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>") `shouldBe` [
        Moon ( 2, 1, -3) (-3,-2, 1),
        Moon ( 1,-8, 0) (-1, 1, 3),
        Moon ( 3,-6, 1) ( 3, 2,-3),
        Moon ( 2, 0, 4) ( 1,-1,-1)
      ]
    
    (!! 99) (run $ parse "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>") `shouldBe` [
        Moon (  8,-12, -9) (-7,  3, 0),
        Moon ( 13, 16, -3) ( 3,-11,-5),
        Moon (-29,-11, -1) (-3,  7, 4),
        Moon ( 16,-13, 23) ( 7,  1, 1)
      ]
  it "examples" $ do
    sum (
        fmap toEnergy (
          (!! 9) $ run (
            parse "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>"
          )
        )
      ) `shouldBe` 179
    sum (
        fmap toEnergy (
          (!! 99) $ run (
            parse "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>"
          )
        )
      ) `shouldBe` 1940

  it "part B" $ do
    partB "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>" `shouldBe` 2772
    partB "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>" `shouldBe` 4686774924