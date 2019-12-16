module Day16 where

import Data.List

basePattern :: Int -> [Int]
basePattern position = drop 1 $ cycle $ concatMap (replicate position) [0, 1, 0, -1]

phase :: [Int] -> [Int]
phase inputs = phase_ 1 inputs
    where
        phase_ :: Int -> [Int] -> [Int]
        phase_ _ [] = []
        phase_ position (head: tail) = ((`mod` 10) $ abs $ sum $ drop (position - 1) $ fmap (uncurry (*)) $ zip inputs $ basePattern position ): phase_ (position + 1) tail


parseInput :: String -> [Int]
parseInput = fmap (read . (:[]))

partA :: String -> Int
partA inputs = toInt $ take 8 $ (!! 99) $ unfoldr step $ parseInput inputs
    where
        step :: [Int] -> Maybe ([Int], [Int])
        step a = let b = phase a in return (b, b)
        toInt :: [Int] -> Int
        toInt = read . concatMap show

partB :: String -> Int
partB str = solution
    where
        inputs = parseInput str
        outputs = (!! 99) $ unfoldr step $ concat $ replicate 4 inputs
        offset = toInt $ take 7 outputs
        solution = toInt $ take 8 $ drop offset outputs
        step :: [Int] -> Maybe ([Int], [Int])
        step a = let b = phase a in return (b, b)
        toInt :: [Int] -> Int
        toInt = read . concatMap show
