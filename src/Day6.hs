module Main where

import Util.File (readLines)
import Data.List (nub)

main :: IO ()
main = do
  lines <- readLines "data/day6/day6.txt"
  print $ day6 4 <$> lines
  print $ day6 14 <$> lines

day6 :: Int -> String -> Int
day6 o = go 1
  where
    go n xs
      | (==o) . length . nub $ take o xs = n+o-1
      | otherwise = go (n+1) (tail xs)
