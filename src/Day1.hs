module Main where

import Util.File (readLines)

main :: IO ()
main = do
  cals <- readLines "data/day1/day1.txt"
  print $ maximum $ sum . fmap read <$> splitOn "" cals

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn sep xs = let (h,t) = go sep xs [] in h : splitOn sep t
  where
    go _ [] acc = (acc, [])
    go s (y:ys) acc
      | y /= s = go s ys (y : acc)
      | otherwise = (acc, ys)
