module Main where

import Util.File (readLines)

main :: IO ()
main = do
  lines <-  readLines "data/day3/day3.txt" 
  print "Day3"
