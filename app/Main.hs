module Main where

import qualified Day01 as D01
import qualified Day02 as D02
import qualified Day03 as D03
import qualified Day04 as D04
import qualified Day05 as D05
import qualified Day06 as D06

run :: String -> String -> (String -> Int) -> IO ()
run day part process = do
  fixtures <- readFile fileName
  print ("day " <> day <> " part " <> part <> " : " <> show (process fixtures))
  where
    fileName = "fixtures/day" <> day <> part <> ".txt"

main :: IO ()
main = do
  run "01" "A" D01.partA
  run "01" "B" D01.partB
  run "02" "A" D02.partA
  run "02" "B" D02.partB
  run "03" "A" D03.partA
  run "03" "B" D03.partB
  run "04" "A" D04.partA
  run "04" "B" D04.partB
  run "05" "A" D05.partA
  run "05" "B" D05.partB
  run "06" "A" D06.partA
  run "06" "B" D06.partB
