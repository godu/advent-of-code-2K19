module Day02 where

import Data.List (find, genericIndex, head, tail, unfoldr)
import Data.List.Extra (splitOn)
import Day05 (getMemory, runProgram, toProgram, update)

partA :: String -> Int
partA input = head $ getMemory $ last $ runProgram program
  where
    memory = update 2 2 $ update 1 12 $ map read $ splitOn "," input
    program = toProgram memory []

cartProd :: [Int] -> [Int] -> [(Int, Int)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

partB :: String -> Int
partB input =
  case find
         (\(x, y) -> try x y 19690720 memory)
         (cartProd [0 .. len] [0 .. len]) of
    Nothing -> undefined
    Just (x, y) -> 100 * x + y
  where
    memory = map read $ splitOn "," input
    len = length memory
    try :: Int -> Int -> Int -> [Int] -> Bool
    try x y z =
      (==) z .
      head .
      getMemory . last . runProgram . (`toProgram` []) . update 2 y . update 1 x
