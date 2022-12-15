{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Lens (ifoldMap)
import Data.Array ( Ix(inRange), Array, listArray, bounds, (!), indices, assocs )
import Data.Char (digitToInt,ord)
import Data.List ((\\),sort)
import Data.Maybe (fromJust,fromMaybe)
import Data.Monoid
import Util.File (readLines)

import qualified Data.Map  as M
import Data.Set (Set)
import qualified Data.Set as S

type Pos = (Int, Int)
type Grid = Array Pos (Char,Int)
type Neighbors = M.Map Pos [Pos]

main = do
  input@(x:xs) <- (fmap . fmap $ elevation) . lines <$> readFile "data/day12/day12.txt"
  let 
    h = length input
    w = length x
    grid = listArray ((0, 0), (h-1,w-1)) $ concat input
    start = fst . Prelude.head $ filter (\((_,_), (c,_)) -> c=='S') $ assocs grid
    starts = fmap ((,0) . fst) $ filter (\((_,_), (c,_)) -> c=='a') $ assocs grid
    end = fst . Prelude.head $ filter (\((_,_), (c,_)) -> c=='E') $ assocs grid
    neighborsMap = neighbors grid
    part1 = bfs neighborsMap [(start,0)]
    part2 = bfs neighborsMap starts
  print starts
  print $ lookup end part1
  print $ lookup end part2
  where
    elevation 'S' = ('S', ord 'a' - 97)
    elevation 'E' = ('E', ord 'z' - 97)
    elevation n = (n, ord n - 97)

{-# inline  at #-}
at :: Grid -> Pos -> Maybe (Char,Int)
at a p
  | inRange (bounds a) p = Just $ a ! p
  | otherwise = Nothing

{-# inline  at' #-}
at' :: Grid -> Pos -> Maybe (Pos, (Char,Int))
at' a p
  | inRange (bounds a) p = Just (p, a ! p)
  | otherwise = Nothing

nb :: Grid -> Pos -> (Char, Int) -> Neighbors
nb g s@(x,y) (c,h) = let
    u = (x, y-1)    
    d = (x, y+1)
    l = (x-1, y)
    r = (x+1, y)
    in if c == 'E' then M.singleton s [] else M.singleton s $ filter (cc . at' g) [u,d,l,r]
    where
      bou = bounds g
      -- (c,h) = fromJust (at g s)
      cc :: Maybe (Pos, (Char, Int)) -> Bool
      cc = maybe False $ liftM2 (&&) (bCheck bou . fst) (hCheck . snd)
      hCheck :: (Char,Int) -> Bool
      hCheck (_,i) = h - i >= -1
      bCheck :: (Pos,Pos) -> Pos -> Bool
      bCheck ((yb, xb),(yb',xb')) (yc,xc) = yc >= yb && xc >= xb && yc <= yb' && xc <= xb'

neighbors :: Grid -> Neighbors
neighbors = nb >>= ifoldMap

bfs nb = go S.empty
  where
    go _ [] = []
    go visited (x@(p,l):xs)
      | x `S.member` visited = go visited xs
      | otherwise = x : go visited' (xs ++ nbs)
      where
        visited' = S.insert x visited
        nbs = (,l+1) <$> M.findWithDefault [] p nb
