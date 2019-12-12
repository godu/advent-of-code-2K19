module Day10 where

import Data.Group
import Data.List
import Data.List.Extra
import Data.Monoid
import Data.Ord
import Data.Ratio
import Data.Semigroup
import Data.Tuple

indexList :: [a] -> [(Int, a)]
indexList = indexList' 0
  where
    indexList' _ [] = []
    indexList' x (head:tail) = (x, head) : indexList' (x + 1) tail

type Vector = (Int, Int)

readMap :: String -> [Vector]
readMap = concatMap (uncurry readRow) . indexList . splitOn "\n"
  where
    readRow :: Int -> String -> [Vector]
    readRow y = createPoints y . filterAsteroid . indexList . fmap (: [])
      where
        filterAsteroid :: [(Int, String)] -> [(Int, String)]
        filterAsteroid = filter ((== "#") . snd)
        createPoints :: Int -> [(Int, String)] -> [Vector]
        createPoints y = map (tuple y . fst)
          where
            tuple y x = (x,y)

addVector :: Vector -> Vector -> Vector
addVector (x, y) (x', y') = (x + x', y + y')

invertVector (x, y) = (-x, -y)

compareVector :: Vector -> Vector -> Ordering
compareVector (x, y) (x', y')
  | x * x' >= 0 && y * y' >= 0 && (x * y') == (x' * y) = (x*x + y*y) `compare` (x'*x' + y'*y')
  | x >= 0 && x' < 0 = LT
  | x < 0 && x' >= 0 = GT
  | otherwise = (x' * y) `compare` (x * y')

groupByAngle :: [Vector] -> [[Vector]]
groupByAngle = groupBy sameAngle . sortBy compareVector
  where
    sameAngle (x, y) (x', y') = x * x' >= 0 && y * y' >= 0 && (x' * y) == (x * y')

selectBestAsteroid :: [Vector] -> (Vector, [[Vector]])
selectBestAsteroid asteroids = maximumOn (length . snd) $ detectAsteroids <$> asteroids
  where
    detectAsteroids :: Vector -> (Vector, [[Vector]])
    detectAsteroids asteroid = (asteroid, fmap (fmap (addVector asteroid)) $ groupByAngle $ map (addVector $ invertVector asteroid) $ filter (/= asteroid) asteroids)

partA :: String -> Int
partA input = length $ snd bestAsteroid
  where
    asteroids = readMap input
    bestAsteroid = selectBestAsteroid asteroids

vaporizeAsteroids :: [[Vector]] -> [Vector]
vaporizeAsteroids [] = []
vaporizeAsteroids ([]:tail) = vaporizeAsteroids tail
vaporizeAsteroids ((head': tail'): tail) = head' : vaporizeAsteroids (tail <> [tail'])

partB :: String -> Int
partB input = calculValue (vaporizedAsteroids !! 199)
  where
    asteroids = readMap input
    bestAsteroid :: (Vector, [[Vector]])
    bestAsteroid = selectBestAsteroid asteroids
    vaporizedAsteroids :: [Vector]
    vaporizedAsteroids = vaporizeAsteroids $ snd bestAsteroid
    calculValue :: Vector -> Int
    calculValue (x, y) = x * 100 + y