module Main where


import Data.Char (ord)
import Data.List (intersect,nub)
import GHC.Unicode (isAsciiLower, isAsciiUpper)
import Util.File (readLines)

main :: IO ()
main = do
  lines <- readLines "data/day3/day3.txt"
  print $ sum $ priority . head . uncurry intersect . halve <$> lines
  print $ sum $ priority . head . intersect' <$> byThree lines

halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = splitAt h xs
  where
  h = length xs `div` 2

priority :: Char -> Int
priority c | isAsciiLower c = ord c - 96
           | isAsciiUpper c = ord c - 38
           | otherwise = error "invalid char"

byThree :: [a] -> [[a]]
byThree [] = []
byThree (x:y:z:ss) = [x,y,z] : byThree ss

intersect' :: [String] -> String
intersect' [] = []
intersect' xs = go xs
  where
    go [] = ['a'..'z'] ++ ['A'..'Z']
    go (x:xs) = x `intersect` go xs

