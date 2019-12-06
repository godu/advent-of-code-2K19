module Day06
  ( partA
  , partB
  ) where

import Data.List.Extra (splitOn, stripInfix)
import qualified Data.Map as M
import qualified Data.Tuple as T
import qualified Data.Set as S
import qualified Data.Maybe as O

type Object = String
type Vertex = (Object, Object)

readInput :: String -> [Vertex]
readInput = O.mapMaybe (stripInfix ")") . splitOn "\n"

partA :: String -> Int
partA input = sum $ toLevel parentMap <$> objects
  where
    parentMap :: M.Map Object Object
    parentMap = M.fromList $ T.swap <$> readInput input
    objects :: [Object]
    objects = S.toList $ S.fromList $ concat $ (\(a, b) -> [a, b]) <$> readInput input
    toLevel :: M.Map Object Object -> Object -> Int
    toLevel parents o = case M.lookup o parents of
      Nothing -> 0
      Just parent -> 1 + toLevel parents parent

partB :: String -> Int
partB input = S.size (S.difference sanPath youPath <> S.difference youPath sanPath)
  where
    parentMap :: M.Map Object Object
    parentMap = M.fromList $ T.swap <$> readInput input
    youPath :: S.Set Object
    youPath = S.fromList $ toPath parentMap "YOU"
    sanPath :: S.Set Object
    sanPath = S.fromList $ toPath parentMap "SAN"
    toPath :: M.Map Object Object -> Object -> [Object]
    toPath parents o = case M.lookup o parents of
      Nothing -> []
      Just parent -> parent : toPath parents parent