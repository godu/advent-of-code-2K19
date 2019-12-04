module Day04
  ( partA
  , partB
  , isASixDigitNumber
  , toDigits
  , hasSameTwoAdjacentDigits
  , hasDigitsOnlyIncrease
  ) where

import Data.List
import Data.List.Extra (splitOn)
import Data.Tuple (swap)

isASixDigitNumber :: Int -> Bool
isASixDigitNumber x
  | x >= 1000000 = False
  | x < 100000 = False
  | otherwise = True

toDigits :: Int -> [Int]
toDigits = unfoldr (step . swap . (`divMod` 10))
  where
    step :: (Int, Int) -> Maybe (Int, Int)
    step (0, 0) = Nothing
    step x = return x

hasSameTwoAdjacentDigits :: Int -> Bool
hasSameTwoAdjacentDigits = any ((>= 2) . length) . group . toDigits

hasDigitsOnlyIncrease :: Int -> Bool
hasDigitsOnlyIncrease = all (uncurry (>=)) . toPairs . toDigits
  where
    toPairs :: [a] -> [(a, a)]
    toPairs xs = zip xs (tail xs)

partA :: String -> Int
partA =
  length .
  filter isASixDigitNumber .
  filter hasSameTwoAdjacentDigits .
  filter hasDigitsOnlyIncrease . range . map read . splitOn "-"
  where
    range :: [Int] -> [Int]
    range (a:b:_) = [a .. b]
    range _ = []

hasOnlyTwoAdjacentDigits :: Int -> Bool
hasOnlyTwoAdjacentDigits = any ((== 2) . length) . group . toDigits

partB :: String -> Int
partB =
  length .
  filter isASixDigitNumber .
  filter hasOnlyTwoAdjacentDigits .
  filter hasDigitsOnlyIncrease . range . map read . splitOn "-"
  where
    range :: [Int] -> [Int]
    range (a:b:_) = [a .. b]
    range _ = []
