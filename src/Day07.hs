module Day07 where

import Data.List
import Data.List.Extra
import qualified Data.Maybe as O
import Day05

runAmplifiers :: [Int] -> [Int] -> [Int] -> [Int]
runAmplifiers memory = foldl (step memory)
  where
    step :: [Int] -> [Int] -> Int -> [Int]
    step memory input setting =
      O.mapMaybe output (runProgram $ toProgram memory (setting : input))

partA :: String -> Int
partA input = maximum (sum . runAmplifiers memory [0] <$> permutations [0 .. 4])
  where
    memory :: [Int]
    memory = read <$> splitOn "," input

partB :: String -> Int
partB input = maximum $ getOutputs <$> permutations [5 .. 9]
  where
    memory :: [Int]
    memory = read <$> splitOn "," input
    getOutputs :: [Int] -> Int
    getOutputs settings = last outputs
      where
        outputs :: [Int]
        outputs = runAmplifiers memory (0 : outputs) settings
