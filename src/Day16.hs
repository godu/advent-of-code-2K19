module Day16 where

import Data.List

applyPattern :: Int -> [Int] -> [Int]
applyPattern position = zipWith (\f x -> f x) pattern_ 
    where
        pattern_ :: [Int -> Int]
        pattern_ = drop (position + 1) $ cycle $ concatMap (replicate (position + 1)) [const 0, id, const 0, negate]

toDigit :: Int -> Int
toDigit = (`mod` 10) . abs

phase :: [Int] -> [Int]
phase signal = zipWith (\i xs -> toDigit $ sum $ applyPattern i xs) [0..] tails_
        where
            tails_ :: [[Int]]
            tails_ = take (length signal) $ tails signal

parseInput :: String -> [Int]
parseInput = fmap (read . (:[]))

step :: [Int] -> Maybe ([Int], [Int])
step a = let b = phase a in return (b, b)
toInt :: [Int] -> Int
toInt = read . concatMap show

partA :: String -> Int
partA inputs = toInt $ take 8 $ (!! 99) $ unfoldr step $ parseInput inputs

partB :: String -> Int
partB str = solution
    where
        inputs = parseInput str
        outputs = (!! 1) $ unfoldr step $ concat $ replicate 10000 inputs
        offset = toInt $ take 7 outputs
        solution = toInt $ take 8 $ drop offset outputs
