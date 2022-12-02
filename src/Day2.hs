module Main where

import Data.Char (ord)
import Util.File (readLines)

data Sign = Rock | Paper | Scissors deriving (Eq,Show)

instance Enum Sign where
  fromEnum Rock = 1
  fromEnum Paper = 2
  fromEnum Scissors = 3
  -- gotta love broken instances
  toEnum 65 = Rock
  toEnum 66 = Paper
  toEnum 67 = Scissors
  toEnum 88 = Rock
  toEnum 89 = Paper
  toEnum 90 = Scissors

main :: IO ()
main = do
  lines <-  readLines "data/day2/day2.txt"
  print $ sum $ uncurry score . normalize <$> lines
  print $ sum $ uncurry score . normalize' <$> lines
  where
    toSign = toEnum . ord
    normalize (a:_:b:_) = (toSign a, toSign b)
    normalize' (a:_:b:_) = let o = toSign a in (o, toSign' b o)

toSign' :: Char -> Sign -> Sign
toSign' 'Y' x = x
toSign' 'X' Rock  = Scissors
toSign' 'Z' Rock  = Paper
toSign' 'X' Paper = Rock
toSign' 'Z' Paper = Scissors
toSign' 'X' Scissors = Paper
toSign' 'Z' Scissors = Rock

outcome :: Sign -> Sign -> Int
outcome Rock Paper = 6
outcome Rock Scissors = 0
outcome Paper Rock = 0
outcome Paper Scissors = 6
outcome Scissors Rock = 6
outcome Scissors Paper = 0
outcome _ _ = 3

score :: Sign -> Sign -> Int
score a b = fromEnum b + outcome a b
