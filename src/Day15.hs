module Day15 where

import Data.List.Extra
import Data.Maybe
import Day05 (toProgram, runProgram, output)
import qualified Data.Set as Set
import Debug.Trace

partA :: String -> Int
partA input = 
  trace (showMap walls (Set.fromList [oxygenSystem])) $
  findSmallestPath walls oxygenSystem
  where
    memory = map read $ splitOn "," input
    (walls, oxygenSystem) = explore memory

data Direction = North | South | West | East
  deriving (Eq, Show)

directionToInt North = 1
directionToInt South = 2
directionToInt West = 3
directionToInt East = 4

intToDirection 1 = North
intToDirection 2 = South
intToDirection 3 = West
intToDirection 4 = East

turnLeft North = East
turnLeft East = South
turnLeft South = West
turnLeft West = North

turnRight East = North
turnRight South = East
turnRight West = South
turnRight North = West

data State = 
  State
    { position :: (Int, Int)
    , direction :: Direction
    , inputs :: [Int]
    , walls :: Set.Set (Int, Int)
    , oxygenSystem :: Maybe (Int, Int)
    , loop :: Int
    }

explore :: [Int] -> (Set.Set (Int, Int), (Int, Int))
explore memory = (walls_, oxygenSystem_)
  where
    program = toProgram memory droidOutputs
    programOutput = catMaybes $ output <$> runProgram program
    initialDroidState = 
        State
          { position = (0, 0)
          , direction = North
          , inputs = programOutput
          , walls = Set.empty
          , oxygenSystem = Nothing
          , loop = 1
          }
    droidSteps = step initialDroidState
    droidOutputs = catMaybes $ snd <$> droidSteps
    lastState = fst $ last droidSteps
    walls_ = walls lastState
    oxygenSystem_ = fromMaybe (0, 0) $ oxygenSystem lastState

step :: State -> [(State, Maybe Int)]
step state = (state, return move) : stepFeedback result state
  where
    move = directionToInt (direction state)
    result = head $ inputs state
    stepFeedback 0 state = 
      step state {
        inputs = tail $ inputs state,
        direction = nextDirection,
        walls = Set.insert (positionTowards (position state) (direction state)) $ walls state
      }
      where
        nextDirection = turnLeft $ direction state
    stepFeedback 1 state =
      step state {
        inputs = tail $ inputs state,
        direction = nextDirection,
        position = nextPosition
      }
      where
        nextPosition = positionTowards (position state) (direction state)
        nextDirection = turnRight $ direction state
    stepFeedback 2 state
      | loop state == 0  = []
      | otherwise = stepFeedback 1 state{
        loop = loop state - 1,
        oxygenSystem = return nextPosition}
      where
        nextPosition = positionTowards (position state) (direction state)

positionTowards :: (Int, Int) -> Direction -> (Int, Int)
positionTowards (x, y) North = (x, y + 1)
positionTowards (x, y) South = (x, y - 1)
positionTowards (x, y) West = (x - 1, y)
positionTowards (x, y) East = (x + 1, y)

showMap :: Set.Set (Int, Int) -> Set.Set (Int, Int) -> String
showMap walls_ oxygenSystem = unlines (showLine <$> reverse [minY .. maxY])
  where
    minX = min 0 $ fromMaybe 0 $ Set.lookupMin $ Set.map fst walls_
    minY = min 0 $ fromMaybe 0 $ Set.lookupMin $ Set.map snd walls_
    maxX = max 0 $ fromMaybe 0 $ Set.lookupMax $ Set.map fst walls_
    maxY = max 0 $ fromMaybe 0 $ Set.lookupMax $ Set.map snd walls_
    showLine y = concatMap (showCell y) [minX .. maxX]
    showCell 0 0 = "O"
    showCell y x | Set.member (x, y) walls_  = "#"
                 | Set.member (x, y) oxygenSystem  = "X"
                 | otherwise = " "

findSmallestPath :: Set.Set (Int, Int) -> (Int, Int) -> Int
findSmallestPath walls end =
  findSmallestPath_ walls end [(0, 0)] 1
  where
    findSmallestPath_ :: Set.Set (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int -> Int
    findSmallestPath_ walls end cells level =
      if end `elem` neighbors
      then level
      else findSmallestPath_ (Set.union (Set.fromList neighbors) walls) end neighbors (level + 1)
      where
        getNeighbors cell = 
          positionTowards cell <$>
          [North, South, East, West]
        neighbors = 
          filter (`notElem` walls) $
          concatMap getNeighbors cells

fillOxygen :: Set.Set (Int, Int) -> (Int, Int) -> Int
fillOxygen walls oxygenSystem = fillOxygen_ walls [oxygenSystem] 0
  where
    fillOxygen_ _ [] minutes = minutes - 1
    fillOxygen_ walls cells minutes =
      fillOxygen_
        (Set.union (Set.fromList neighbors) walls)
        neighbors
        (minutes + 1)
      where
        getNeighbors cell = 
          positionTowards cell <$>
          [North, South, East, West]
        neighbors = 
          filter (`notElem` walls) $
          concatMap getNeighbors cells

partB :: String -> Int
partB input = 
  fillOxygen walls oxygenSystem
  where
    memory = map read $ splitOn "," input
    (walls, oxygenSystem) = explore memory
