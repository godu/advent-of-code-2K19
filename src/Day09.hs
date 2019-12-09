module Day09 where

import Data.List
import Data.List.Extra
import qualified Data.Maybe as O
import Day05

runA :: [Int] -> [Int] -> [Int]
runA memory inputs = O.mapMaybe output (runProgram $ toProgram memory inputs)

partA :: String -> Int
partA input = sum outputs
  where
    memory = read <$> splitOn "," input
    outputs = runA memory [1]

partB :: String -> Int
partB input = sum outputs
  where
    memory = read <$> splitOn "," input
    outputs = runA memory [2]