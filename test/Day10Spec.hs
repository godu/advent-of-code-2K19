module Day10Spec where

import Data.Group
import Data.List
import Data.List.Extra
import Data.Ord
import Data.Tuple
import Day10
import Test.Hspec

spec :: Spec
spec = do
  it "readMap" $
    readMap ".#..#\n.....\n#####\n....#\n...##" `shouldBe` 
      [ (1, 0)
      , (4, 0)
      , (0, 2)
      , (1, 2)
      , (2, 2)
      , (3, 2)
      , (4, 2)
      , (4, 3)
      , (3, 4)
      , (4, 4)
      ]
  it "addVector" $ ((1 ,1) `addVector` (4, 3)) `shouldBe` (5, 4)
  it "invertVector" $ invertVector (1, 1) `shouldBe` (-1, -1)
  it "compareVector" $ do
     (0, -1) `compareVector`  (1, -1) `shouldBe` LT
     (1, -1) `compareVector` ( 1 ,0 )`shouldBe` LT
     (1 ,0 )`compareVector`  (1, 1 )`shouldBe` LT
     (1 ,1) `compareVector`  (0, 1 )`shouldBe` LT
     (0 ,1) `compareVector`  (-1, 1) `shouldBe` LT
     (-1, 1) `compareVector` (-1, 0) `shouldBe` LT
     (-1, 0) `compareVector` (-1, -1) `shouldBe` LT
     (-1, -1) `compareVector` (0, -1) `shouldBe` GT

     (1, 1) `compareVector`  (2, 2) `shouldBe` LT
     (2, 2) `compareVector`  (1, 1) `shouldBe` GT
     (1, 1) `compareVector`  (-2, -2) `shouldBe` LT
     (-2, -2) `compareVector` (1, 1) `shouldBe` GT

     (1, 2) `compareVector`  (2, 4) `shouldBe` LT
     (2, 4) `compareVector`  (1, 2) `shouldBe` GT

     sortBy compareVector [
          (2, 5),
          (1, 4),
          (1, 2),
          (2 ,4)
      ] `shouldBe` [
          (1, 2),
          (2 ,4),
          (2, 5),
          (1, 4)
      ]
  it "groupByAngle" $ do
    groupByAngle [
         (2, 5),
         (1, 4),
         (1, 2),
         (2 ,4)
      ] `shouldBe` [
        [ (1, 2),  (2, 4)],
        [ (2, 5)],
        [ (1, 4)]
      ]
    groupByAngle [
         (1 ,2),
        ( 2, 4)
      ] `shouldBe` [
        [ (1, 2),  (2 ,4)]
      ]
    groupByAngle [
         (2, 4),
         (1, 2),
         (2, 5)
      ] `shouldBe` [
        [ (1, 2),  (2, 4)],
        [ (2, 5)]
      ]

  it "partA" $ do
    partA ".#..#\n.....\n#####\n....#\n...##" `shouldBe` 8
    partA "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####" `shouldBe` 33
    partA "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###." `shouldBe` 35
    partA ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#.." `shouldBe` 41
    partA ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##" `shouldBe` 210
  
  it "vaporizeAsteroids" $ 
    vaporizeAsteroids [
      [ (1, 2),  (2, 4)],
      [ (2, 5)],
      [ (1, 4)]
    ] `shouldBe` [ (1, 2), (2, 5), (1, 4), (2, 4) ]

  it "partB" $
    partB ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"  `shouldBe` 802