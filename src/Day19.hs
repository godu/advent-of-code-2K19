module Day19 where

import Day05
import Data.List.Extra
import qualified Data.Maybe as Maybe

scanCell :: [Int] -> (Int, Int) -> [Int]
scanCell memory (x, y) = outputs
  where
    inputs = [x, y]
    program :: Program
    program = toProgram memory inputs
    outputs :: [Int]
    outputs = Maybe.mapMaybe output $ runProgram program

scanArea :: [Int] -> (Int, Int) -> [Int]
scanArea memory (x, y) = concatMap scan points
  where
    xs = [0 .. (x-1)]
    ys = [0 .. (y-1)]
    points :: [(Int, Int)]
    points = concatMap (\y -> fmap (\x -> (x, y)) xs) ys
    scan :: (Int, Int) -> [Int]
    scan = scanCell memory

partA :: String -> Int
partA input = sum $ scanArea memory (50, 50)
  where
    memory = map read $ splitOn "," input

partB :: String -> Int
partB input = undefined
