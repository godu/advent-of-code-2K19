module Day11 where

import Prelude hiding (Left, Right)
import Data.List
import Data.List.Extra 
import Data.Ix
import qualified Data.Map as M
import qualified Data.Maybe as O
import Day05
import Debug.Trace

data Direction = Up | Down | Left | Right deriving (Show, Eq)

turn :: Direction -> Int -> Direction
turn Up 0 = Left
turn Left 0 = Down
turn Down 0 = Right
turn Right 0 = Up
turn Up 1 = Right
turn Right 1 = Down
turn Down 1 = Left
turn Left 1 = Up

data Color = White | Black deriving (Show, Eq)


showColor :: Color -> String
showColor White = "█"
showColor Black = " "

toColor :: Int -> Color
toColor 0 = Black
toColor 1 = White

fromColor :: Color -> Int
fromColor Black = 0
fromColor White = 1

type Panel =  M.Map (Int, Int) Color
type State = ((Int, Int), Direction, Panel)

parseInput :: String -> [Int]
parseInput = fmap read . splitOn ","

trd (_, _, x) = x

showPanel :: Panel -> String
showPanel panel = unlines $ showRow panel <$> range (fst $ fst bounces, fst $ snd bounces)
  where
    bounces = foldl step ((0, 0), (0, 0)) (M.keys panel)
    step :: ((Int, Int), (Int, Int)) -> (Int, Int) -> ((Int, Int), (Int, Int))
    step ((minX, minY), (maxX, maxY)) (x, y) = ((min minX x, min minY y), (max maxX x, max maxY y))
    showRow :: Panel -> Int -> String
    showRow panel x = concatMap (\y -> showColor $ O.fromMaybe Black $ M.lookup (x, y) panel) $ range (snd $ fst bounces, snd $ snd bounces)

run :: Color -> String -> Panel
run initialColor input = finalState
  where
    memory = parseInput input
    program = toProgram memory (fromColor initialColor: inputs)
    outputs :: [Int]
    outputs =  O.mapMaybe output $ runProgram program

    initialState = ((0, 0), Up, M.empty)
    robots = unfoldr step (chunksOf 2 outputs, initialState)
    inputs :: [Int]
    inputs = fmap (fromColor . fst) robots
    finalState = snd $ last robots

    step :: ([[Int]], State) ->  O.Maybe ((Color, Panel), ([[Int]], State))
    step ([], _) = Nothing
    step (outputs:tail, (position, direction, panel)) =  
      Just (
          (nextColor , nextPanel),
          (tail, (nextPosition, nextDirection, nextPanel))
        )
      where
        nextDirection :: Direction
        nextDirection = stepDirection direction outputs
        nextPanel :: Panel
        nextPanel = stepPanel position panel outputs
        nextPosition :: (Int, Int)
        nextPosition = forward position nextDirection 
        nextColor :: Color
        nextColor = O.fromMaybe Black $ M.lookup nextPosition nextPanel
    stepDirection direction [_, rotation] = turn direction rotation
    stepPanel position panel [color, _] = M.insert position (toColor color) panel
    forward (x, y) Up = (x - 1, y)
    forward (x, y) Down = (x + 1, y)
    forward (x, y) Left = (x, y - 1)
    forward (x, y) Right = (x, y + 1)

partA :: String -> Int
partA = length . run Black


partB :: String -> Int
partB input = trace (showPanel $ run White input) 0 