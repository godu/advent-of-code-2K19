module Day05
  ( partA
  , partB
  , runProgram
  ) where

import Data.List (find, genericIndex, head, tail)
import Data.List.Extra (splitOn)
import Day04 (toDigits)

type Memory = [Int]

type Position = Int

type Input = Int

type Output = Int

type Mode = Int

runProgram :: Memory -> [Input] -> [Output]
runProgram memory inputs = go memory inputs 0
  where
    go :: Memory -> [Input] -> Position -> [Output]
    go memory inputs position =
      case toDigits (index position memory) <> [0, 0, 0] of
        (1:0:x:y:_) ->
          go (addition (x, y) memory position) inputs (position + 4)
        (2:0:x:y:_) ->
          go (multiplication (x, y) memory position) inputs (position + 4)
        (3:0:_) ->
          go (scan memory (head inputs) position) (tail inputs) (position + 2)
        (4:0:x:_) -> print x memory position : go memory inputs (position + 2)
        (5:0:x:y:_) ->
          (\(position, memory) -> go memory inputs position) $
          true (x, y) memory position
        (6:0:x:y:_) ->
          (\(position, memory) -> go memory inputs position) $
          false (x, y) memory position
        (7:0:x:y:_) -> go (less (x, y) memory position) inputs (position + 4)
        (8:0:x:y:_) -> go (equals (x, y) memory position) inputs (position + 4)
        _ -> []
    getValue :: Mode -> Position -> Memory -> Int
    getValue 0 position memory = index (index position memory) memory
    getValue 1 position memory = index position memory
    addition :: (Mode, Mode) -> Memory -> Position -> Memory
    addition (x, y) memory position =
      update
        (index (position + 3) memory)
        (getValue x (position + 1) memory + getValue y (position + 2) memory)
        memory
    multiplication :: (Mode, Mode) -> Memory -> Position -> Memory
    multiplication (x, y) memory position =
      update
        (index (position + 3) memory)
        (getValue x (position + 1) memory * getValue y (position + 2) memory)
        memory
    true :: (Mode, Mode) -> Memory -> Position -> (Position, Memory)
    true (x, y) memory position =
      if getValue x (position + 1) memory == 0
        then (position + 3, memory)
        else (getValue y (position + 2) memory, memory)
    false :: (Mode, Mode) -> Memory -> Position -> (Position, Memory)
    false (x, y) memory position =
      if getValue x (position + 1) memory == 0
        then (getValue y (position + 2) memory, memory)
        else (position + 3, memory)
    less :: (Mode, Mode) -> Memory -> Position -> Memory
    less (x, y) memory position =
      update
        (index (position + 3) memory)
        (if getValue x (position + 1) memory < getValue y (position + 2) memory
           then 1
           else 0)
        memory
    equals :: (Mode, Mode) -> Memory -> Position -> Memory
    equals (x, y) memory position =
      update
        (index (position + 3) memory)
        (if getValue x (position + 1) memory == getValue y (position + 2) memory
           then 1
           else 0)
        memory
    scan :: Memory -> Input -> Position -> Memory
    scan memory input position =
      update (index (position + 1) memory) input memory
    print :: Mode -> Memory -> Position -> Output
    print mode memory position = getValue mode (position + 1) memory

index :: Int -> [a] -> a
index i xs = genericIndex xs i

update :: Int -> a -> [a] -> [a]
update 0 x (head:tail) = x : tail
update i x (head:tail) = head : update (i - 1) x tail

partA :: String -> Int
partA input = sum $ runProgram memory [1]
  where
    memory = map read $ splitOn "," input

partB :: String -> Int
partB input = sum $ runProgram memory [5]
  where
    memory = map read $ splitOn "," input
