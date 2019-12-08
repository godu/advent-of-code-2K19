module Day08 where

import Data.Char
import Data.List
import Data.List.Extra
import Debug.Trace

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n xs = take n xs : groupN n (drop n xs)

checkSum :: (Int, Int) -> String -> Int
checkSum (x, y) = toCRC . minimumOn (count 0) . toLayer . map digitToInt
  where
    toLayer :: [Int] -> [[Int]]
    toLayer = groupN (x * y)
    count :: Int -> [Int] -> Int
    count n = length . filter (== n)
    toCRC :: [Int] -> Int
    toCRC layer = count 1 layer * count 2 layer

partA :: String -> Int
partA = checkSum (25, 6)

mergePixels :: Int -> Int -> Int
mergePixels 2 b = b
mergePixels a _ = a

mergeLayers :: [Int] -> [Int] -> [Int]
mergeLayers = zipWith mergePixels

renderVisibleImage :: (Int, Int) -> String -> [Int]
renderVisibleImage (x, y) = foldl1 mergeLayers . toLayer . map digitToInt
  where
    toLayer :: [Int] -> [[Int]]
    toLayer = groupN (x * y)

showLayer :: [[Int]] -> String
showLayer xs = unlines $ fmap (concatMap showPixel) xs
  where
    showPixel 0 = "█"
    showPixel 1 = " "
    showPixel _ = " "

partB :: String -> Int
partB input = sum $ map sum $ trace (showLayer layer) layer
  where
    layer :: [[Int]]
    layer = groupN 25 $ renderVisibleImage (25, 6) input
