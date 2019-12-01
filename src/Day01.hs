module Day01
  ( partA
  , partB
  ) where

import Data.List

toFuel :: Int -> Int
toFuel = (+) (-2) . (`div` 3)

partA :: String -> Int
partA = sum . map (toFuel . read) . lines

partB :: String -> Int
partB = sum . map (recToFuel . read) . lines
  where
    recToFuel :: Int -> Int
    recToFuel = sum . unfoldr (continueIfNotZero . toFuel)
    continueIfNotZero x
      | x <= 0 = Nothing
      | otherwise = Just (x, x)
