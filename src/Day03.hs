module Day03
  ( partA
  , partB
  , toManhattanDistance
  , unfoldWire
  , readInstruction
  ) where

import Data.Function (on)
import Data.List
import Data.List.Extra (splitOn)
import Data.Maybe (fromMaybe)
import Data.Set (fromList, intersection, toList)

type Vector = (Int, Int)

type Wire = [Vector]

toManhattanDistance :: Vector -> Int
toManhattanDistance (x, y) = abs x + abs y

type Instruction = Vector

type Instructions = [Instruction]

unfoldWire :: Instructions -> Wire
unfoldWire instructions = unfoldr produce (instructions, (0, 0))
  where
    produce :: (Instructions, Vector) -> Maybe (Vector, (Instructions, Vector))
    produce ((x, y):tail, (ox, oy))
      | x > 0 = Just ((ox + 1, oy), ((x - 1, y) : tail, (ox + 1, oy)))
      | x < 0 = Just ((ox - 1, oy), ((x + 1, y) : tail, (ox - 1, oy)))
      | y > 0 = Just ((ox, oy + 1), ((x, y - 1) : tail, (ox, oy + 1)))
      | y < 0 = Just ((ox, oy - 1), ((x, y + 1) : tail, (ox, oy - 1)))
      | otherwise = produce (tail, (ox, oy))
    produce _ = Nothing

readInstruction :: String -> Instruction
readInstruction ('U':tail) = (0, read tail)
readInstruction ('D':tail) = (0, -(read tail))
readInstruction ('L':tail) = (read tail, 0)
readInstruction ('R':tail) = (-(read tail), 0)
readInstruction _ = (0, 0)

intersectList :: [Vector] -> [Vector] -> [Vector]
intersectList x y = toList $ (intersection `on` fromList) x y

partA :: String -> Int
partA = process . map (unfoldWire . map readInstruction . splitOn ",") . lines
  where
    process :: [Wire] -> Int
    process (wireA:wireB:_) =
      minimum $ map toManhattanDistance $ intersectList wireA wireB
    process _ = 0

partB :: String -> Int
partB = process . map (unfoldWire . map readInstruction . splitOn ",") . lines
  where
    process :: [Wire] -> Int
    process (wireA:wireB:_) =
      minimum $
      map (\x -> position wireA x + position wireB x) $
      intersectList wireA wireB
    process _ = 0
    position :: Wire -> Vector -> Int
    position wire x = maybe 0 (1 +) (elemIndex x wire)
