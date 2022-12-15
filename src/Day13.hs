{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Char (isDigit)
import Data.List (sort)
import Data.List.Split (splitOn)
data P = N Int | L [P] deriving (Read, Show)

main = do
  input <- fmap lines . splitOn "\n\n" <$> readFile "data/day13/day13.txt"
  print $ part1 $ parse input
  print $ part2 $ parse input

part1 x = sum $ fmap fst $ filter (\(_,b) -> b == LT) $ zip [1..] $ fmap (uncurry check . shit) x
part2 x = let pks = sort (concat x ++ dividers) in product [ i | (i,p) <- zip [1..] pks, p `elem` dividers ]

dividers = [L [L [N 2]],L [L [N 6]]]

parse i = fmap ((read @P) . concatMap cheat . (\xs -> zip ('[':init xs) xs)) <$> i

cheat (p,d)
  | d == '[' = "L ["
  | isDigit d && not (isDigit p) = "N " ++ [d]
  | otherwise = [d]

check :: P -> P -> Ordering
check (N x) (N y) = x `compare` y
check (L []) (L []) = EQ
check (L []) _ = LT
check _ (L []) = GT
check (N x) (L ys) = check (L [N x]) (L ys)
check (L ys) (N y) = check (L ys) (L [N y])
check (L (x:xs)) (L (y:ys)) = check x y <> check (L xs) (L ys)

shit :: [P] -> (P,P)
shit [xs,ys] = (xs,ys)
shit _ = error "fuck"

instance Eq P where
  x == y = check x y == EQ

instance Ord P where
  compare = check
