module Main where

import qualified Day01 as D01
import qualified Day02 as D02
import qualified Day03 as D03

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
