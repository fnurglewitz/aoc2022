{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Control.Lens (ifoldMap)
import Data.Array ( Ix(inRange), Array, listArray, bounds, (!), indices, assocs )
import Data.Char (digitToInt,ord)
import Data.List (span)
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
  input@(x:xs) <- (fmap . fmap $ elevation) . lines <$> readFile "data/day12/day12_ex.txt"
  let 
    h = length input
    w = length x
    grid = listArray ((0, 0), (h-1,w-1)) $ concat input
    start = fst . head $ filter (\((_,_), (c,_)) -> c=='S') $ assocs grid
    neighborsMap = neighbors grid
  print $ length $ S.elems $ explore neighborsMap start
  where
    elevation 'S' = ('S', ord 'a' - 97)
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
    in if c == 'E' then M.empty else M.singleton s $ filter (cc . at' g) [u,d,l,r]
    where
      bou = bounds g
      -- (c,h) = fromJust (at g s)
      cc :: Maybe (Pos, (Char, Int)) -> Bool
      cc = maybe False $ liftM2 (&&) (bCheck bou . fst) (hCheck . snd)
      hCheck :: (Char,Int) -> Bool
      hCheck (_,i) = h - i <= 1
      bCheck :: (Pos,Pos) -> Pos -> Bool
      bCheck ((yb, xb),(yb',xb')) (yc,xc) = yc >= yb && xc >= xb && yc <= yb' && xc <= xb'

neighbors :: Grid -> Neighbors
neighbors = nb >>= ifoldMap

explore nb = go mempty
  where
    go s p
      | p `S.member` s = []
      | otherwise = let
          visited = S.insert p s
          toVisit = M.findWithDefault mempty p nb
          in fmap ((p:) . go visited) toVisit

  {-
explore :: Neighbors -> Pos -> Set Pos
explore nb = go mempty
    where
        go s p
            | p `S.member` s = mempty
            | otherwise = let 
                visited = S.insert p s
                toVisit = M.findWithDefault mempty p nb
                in visited <> foldMap (go visited) toVisit
-}
