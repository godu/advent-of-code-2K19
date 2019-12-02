module Day02
  ( partA
  , partB
  , runProgram
  ) where

import Data.List (find, genericIndex)
import Data.List.Extra (splitOn)

type Memory = [Int]

type Position = Int

runProgram :: Memory -> Memory
runProgram memory = go memory 0
  where
    go :: Memory -> Position -> Memory
    go memory position =
      case index position memory of
        1 -> go (addition memory position) (position + 4)
        2 -> go (multiplication memory position) (position + 4)
        _ -> memory
    addition :: Memory -> Position -> Memory
    addition memory position =
      update
        (index (position + 3) memory)
        (index (index (position + 1) memory) memory +
         index (index (position + 2) memory) memory)
        memory
    multiplication :: Memory -> Position -> Memory
    multiplication memory position =
      update
        (index (position + 3) memory)
        (index (index (position + 1) memory) memory *
         index (index (position + 2) memory) memory)
        memory

index :: Int -> [a] -> a
index i xs = genericIndex xs i

update :: Int -> a -> [a] -> [a]
update 0 x (head:tail) = x : tail
update i x (head:tail) = head : update (i - 1) x tail

partA :: String -> Int
partA = index 0 . runProgram . update 2 2 . update 1 12 . map read . splitOn ","

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
    memory :: Memory
    memory = map read $ splitOn "," input
    len = length memory
    try :: Int -> Int -> Int -> Memory -> Bool
    try x y z = (==) z . index 0 . runProgram . update 2 y . update 1 x
