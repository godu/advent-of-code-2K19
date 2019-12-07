module Day05 where

import Data.List (find, genericIndex, head, tail, unfoldr)
import Data.List.Extra (splitOn)

data Program =
  Program
    { memory :: [Int]
    , position :: Int
    , inputs :: [Int]
    , outputs :: [Int]
    }
  deriving (Show, Eq)

getMemory = memory

data Mode_
  = Position
  | Immediate

toProgram :: [Int] -> [Int] -> Program
toProgram memory inputs =
  Program {memory = memory, inputs = inputs, outputs = [], position = 0}

runProgram :: Program -> [Program]
runProgram = unfoldr step
  where
    step program =
      case getOpCode program of
        1 -- +
         ->
          return $
          toTuple $
          forward 4 $
          write
            (getArgumentAdress 3 program)
            (read (getArgumentAdress 1 program) program +
             read (getArgumentAdress 2 program) program)
            program
        2 -- *
         ->
          return $
          toTuple $
          forward 4 $
          write
            (getArgumentAdress 3 program)
            (read (getArgumentAdress 1 program) program *
             read (getArgumentAdress 2 program) program)
            program
        3 -- scan
         ->
          return $
          toTuple $
          forward 2 $
          write (getArgumentAdress 1 program) (head $ inputs program) $
          program {inputs = tail $ inputs program}
        4 -- print
         ->
          return $
          toTuple $
          forward 2 $
          program
            { outputs =
                read (getArgumentAdress 1 program) program : outputs program
            }
        5 -- jump if true
         ->
          return $
          toTuple $
          if read (getArgumentAdress 1 program) program /= 0
            then program {position = read (getArgumentAdress 2 program) program}
            else forward 3 program
        6 -- jump if false
         ->
          return $
          toTuple $
          if read (getArgumentAdress 1 program) program == 0
            then program {position = read (getArgumentAdress 2 program) program}
            else forward 3 program
        7 -- <
         ->
          return $
          toTuple $
          forward 4 $
          write
            (getArgumentAdress 3 program)
            (if read (getArgumentAdress 1 program) program <
                read (getArgumentAdress 2 program) program
               then 1
               else 0)
            program
        8 -- ==
         ->
          return $
          toTuple $
          forward 4 $
          write
            (getArgumentAdress 3 program)
            (if read (getArgumentAdress 1 program) program ==
                read (getArgumentAdress 2 program) program
               then 1
               else 0)
            program
        99 -> Nothing
    toTuple :: a -> (a, a)
    toTuple a = (a, a)
    read :: Int -> Program -> Int
    read adress program = index adress (memory program)
    write :: Int -> Int -> Program -> Program
    write adress value program =
      program {memory = update adress value (memory program)}
    forward :: Int -> Program -> Program
    forward n program = program {position = position program + n}
    getOpCode :: Program -> Int
    getOpCode program = index (position program) (memory program) `mod` 100
    getMode :: Int -> Program -> Mode_
    getMode n program =
      case (`mod` 10) $
           (`div` (10 ^ (n + 1))) $ index (position program) (memory program) of
        0 -> Position
        _ -> Immediate
    getArgumentAdress :: Int -> Program -> Int
    getArgumentAdress n program =
      case getMode n program of
        Position -> read argumentPosition program
        Immediate -> argumentPosition
      where
        argumentPosition = position program + n

index :: Int -> [a] -> a
index i xs = genericIndex xs i

update :: Int -> a -> [a] -> [a]
update 0 x (head:tail) = x : tail
update i x (head:tail) = head : update (i - 1) x tail

partA :: String -> Int
partA input = sum $ outputs $ last $ runProgram program
  where
    memory = map read $ splitOn "," input
    program = toProgram memory [1]

partB :: String -> Int
partB input = sum $ outputs $ last $ runProgram program
  where
    memory = map read $ splitOn "," input
    program = toProgram memory [5]
