{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array ( Ix(inRange), Array, listArray, bounds, (!), indices )
import Data.Char (digitToInt)
import Data.List (span)
import Data.Maybe (fromJust)
import Data.Monoid

import Util.File (readLines)

type Pos = (Int, Int)
type Grid = Array Pos Int

main = do
  input@(x:xs) <- (fmap . fmap $ digitToInt) . lines <$> readFile "data/day8/day8.txt"
  let 
    h = length input
    w = length x
    grid = listArray ((1, 1), (h,w)) $ concat input
  print $ length $ filter (==True) $ fmap (or . fmap fst) $ scene h w grid <$> indices grid
  print $ maximum $ fmap (getProduct . foldMap (Product . snd)) $ scene h w grid <$> indices grid

{-# inline  at #-}
at :: Grid -> Pos -> Maybe Int
at a p
  | inRange (bounds a) p = Just $ a ! p
  | otherwise = Nothing

scene h w grid p@(ph,pw)
  | ph == 1 || pw == 1 || ph == h || pw == w = [(True,0)]
  | otherwise = [up,left,down,right] <*> [p]
  where
    val = fromJust . at grid $ p
    zumzum f xs = 
      let elems = fromJust . at grid . f <$> xs
       in (all (<val) elems, length $ take' val elems)
    up (h',w') = zumzum (,w') (reverse [1..(h'-1)])
    down (h',w') = zumzum (,w')  [(h'+1)..h]
    left (h',w') = zumzum (h',) (reverse [1..(w'-1)])
    right (h',w') = zumzum (h',) [(w'+1)..w]

take' :: Int -> [Int] -> [Int]
take' v [] = []
take' v (x:xs)
  | v <= x = [x]
  | otherwise = x : take' v xs
