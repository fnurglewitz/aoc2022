module Main where

import Data.List (sortBy)
import Util.File (readLines)

main :: IO ()
main = do
  cals <- readLines "data/day1/day1.txt"
  let calories = sum . fmap read <$> splitOn "" cals
  print $ maximum calories
  print $ sum . take 3 $ sortBy (flip compare) calories

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn sep xs = let (h,t) = go sep xs [] in h : splitOn sep t
  where
    go _ [] acc = (acc, [])
    go s (y:ys) acc
      | y /= s = go s ys (y : acc)
      | otherwise = (acc, ys)
