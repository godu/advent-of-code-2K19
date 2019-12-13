module Day12 where

import Debug.Trace
import Data.List
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

type Vector3 = (Int, Int, Int)

data Moon = Moon Vector3 Vector3 deriving (Show, Eq)

instance Read Moon where
  readsPrec _ str = toMoon (getAllTextMatches (str =~ "[0-9-]+") :: [String])
    where
      toMoon :: [String] -> [(Moon, String)]
      toMoon [x, y , z]= [(Moon (read x, read y, read z) (0, 0, 0), "")] 
      toMoon _ = []


position :: Moon -> Vector3
position (Moon position _)= position

velocity :: Moon -> Vector3
velocity (Moon _ velocity)= velocity

updateVelocity :: [Moon] -> [Moon]
updateVelocity = mapWithOthers (foldl step)
  where
    step (Moon (x, y, z) (vx, vy, vz)) (Moon (x', y', z') _) = Moon
        (x, y, z)
        ( vx + deltaVelocity x x'
        , vy + deltaVelocity y y'
        , vz + deltaVelocity z z' )
    deltaVelocity a b =
      case compare a b of
       LT -> 1
       EQ -> 0
       GT -> -1

updatePosition :: [Moon] -> [Moon]
updatePosition = fmap updatePosition_
  where
    updatePosition_ :: Moon -> Moon
    updatePosition_ (Moon (x, y, z) (vx, vy, vz)) =
      Moon (x + vx, y + vy, z + vz) (vx, vy, vz)

mapWithOthers :: (a -> [a] -> a) -> [a] -> [a]
mapWithOthers f = fmap (uncurry f) . allPairs
  where
    allPairs (head: tail) =
      take (length (head: tail)) $ (head, tail) : allPairs (tail <> [head])

printDebug :: Show a => a -> a
printDebug x = trace (show x) x

run :: [Moon] -> [[Moon]]
run = unfoldr step
  where
    step :: [Moon] -> Maybe ([Moon], [Moon])
    step moons = return $ double $ updatePosition $ updateVelocity moons
    double x = (x, x)

toEnergyPotential :: Moon -> Int
toEnergyPotential (Moon (x, y, z) _) = abs x + abs y + abs z

toEnergyKinetic :: Moon -> Int
toEnergyKinetic (Moon _ (vx, vy, vz)) = abs vx + abs vy + abs vz

toEnergy :: Moon -> Int
toEnergy moon = toEnergyPotential moon * toEnergyKinetic moon

parse :: String -> [Moon]
parse = fmap read . lines

partA :: String -> Int
partA input = sum $ fmap toEnergy $ (!! 999) $ run $ parse input

mapi :: (a -> Int -> b) -> [a] -> [b]
mapi f arr = mapi_ f arr 0
  where
    mapi_ f (head: tail) i = f head i : mapi_ f tail (i+1)
    mapi_ _ _ _ = []

partB :: String -> Int
partB input = (*) 2 $ foldl1 lcm [xSemiPeriod, ySemiPeriod, zSemiPeriod]
  where
    moons = parse input
    steps = run moons
    semiPeriod :: (Moon -> Int) -> [[Moon]] -> Int
    semiPeriod f steps =
      (+) 1 $ length $ takeWhile (not . all (== 0)) $ fmap f <$> steps
    xSemiPeriod :: Int
    xSemiPeriod = semiPeriod ((\(vx, _, _) -> vx) . velocity) steps
    ySemiPeriod :: Int
    ySemiPeriod = semiPeriod ((\(_, vy, _) -> vy) . velocity) steps
    zSemiPeriod :: Int
    zSemiPeriod = semiPeriod ((\(_, _, vz) -> vz) . velocity) steps