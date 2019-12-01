module Main where

import Day01

run :: String -> String -> (String -> Int) -> IO ()
run day part process = do
  fixtures <- readFile fileName
  print ("day " <> day <> " part " <> part <> " : " <> show (process fixtures))
  where
    fileName = "fixtures/day" <> day <> part <> ".txt"

main :: IO ()
main = do
  run "01" "A" partA
  run "01" "B" partB
