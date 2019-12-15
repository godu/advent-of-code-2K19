module Day14 where

import Data.List.Extra
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Tuple
import Debug.Trace

type Chemical = String

type Quantity = Int

type Reactions = Map.Map Chemical (Quantity, [(Quantity, Chemical)])

type Wallet = Map.Map Chemical Quantity

parse :: String -> (Quantity, Chemical)
parse = (\(a, b) -> (read a, b)) . Maybe.fromJust . stripInfix " "

parseRow :: String -> (Chemical, (Quantity, [(Quantity, Chemical)]))
parseRow str = (snd result, (fst result, ingredients))
  where
    (ingredientsStr, resultStr) = Maybe.fromJust $ stripInfix " => " str
    result = parse resultStr
    ingredients = parse <$> splitOn ", " ingredientsStr

parseInput :: String -> Reactions
parseInput = Map.fromList . fmap parseRow . lines

convert :: Reactions -> Chemical -> Maybe (Quantity, [(Quantity, Chemical)])
convert reactions chemical = Map.lookup chemical reactions

resolve :: Reactions -> Wallet -> Wallet
resolve reactions wallet =
  case pickFirst wallet of
    Nothing -> wallet
    Just (quantity, chemical) ->
      let (requiredQuantity, results) =
            Maybe.fromJust $ convert reactions chemical
          times =
            (\(m, d) ->
               m +
               if d == 0
                 then 0
                 else 1)
              (quantity `divMod` requiredQuantity)
       in resolve reactions $
          mergeWallet
            wallet
            (Map.fromList $
             (\(c, q) -> (c, q * times)) <$>
             ((chemical, -requiredQuantity) : (swap <$> results)))
  where
    pickFirst :: Wallet -> Maybe (Quantity, Chemical)
    pickFirst wallet =
      Maybe.listToMaybe $
      fmap swap $
      take 1 $
      Map.toList $ Map.filterWithKey (\k v -> v > 0 && k /= "ORE") wallet

mergeWallet :: Wallet -> Wallet -> Wallet
mergeWallet a b = foldr update a $ Map.toList b
  where
    update :: (Chemical, Quantity) -> Wallet -> Wallet
    update (k, a) w = Map.insert k (a + prevValue) w
      where
        prevValue :: Quantity
        prevValue = Maybe.fromMaybe 0 (Map.lookup k w)

partA :: String -> Int
partA input = Maybe.fromMaybe 0 $ Map.lookup "ORE" wallet
  where
    reactions = parseInput input
    wallet = resolve reactions $ Map.fromList [("FUEL", 1)]

resolveFuel :: Reactions -> Quantity -> Quantity
resolveFuel reactions ore = resolveFuel_ 1000000000000 0 Map.empty
  where
    resolveFuel_ step fuelCount wallet =
      if ore < Maybe.fromMaybe 0 (Map.lookup "ORE" nextWallet)
        then if step == 1
               then fuelCount
               else resolveFuel_ (step `div` 10) fuelCount wallet
        else resolveFuel_ step (fuelCount + step) nextWallet
      where
        nextWallet =
          resolve reactions $ mergeWallet wallet $ Map.fromList [("FUEL", step)]

partB :: String -> Int
partB input = resolveFuel reactions 1000000000000
  where
    reactions = parseInput input
