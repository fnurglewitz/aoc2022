{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import qualified Control.Monad.Trans.State.Lazy as S
import Data.Array
import Data.List (sortBy, transpose, intercalate)
import Data.List.Split (splitOn,chunksOf)
import Data.Maybe (fromJust, fromMaybe)
import Util.File (readLines)
import GHC.Arr (listArray)

type Pos = (Int,Int)
type Grid = Array Pos Char

main :: IO ()
main = do
  lines <- readLines "data/day14/day14.txt"
  let x = concatMap expand $ fmap (f . splitOn ",") . splitOn " -> " <$> lines
      hMax = maximum $ snd <$> x
      peak = ((500,0),'.')
      bottom = [ ((i,hMax+2),'#') | i <- [-1000..1000] ]
      grid = accumArray (\_ x -> x) '.' ((-1000,0),(1000,hMax+2)) (peak : bottom ++ ((,'#') <$> x))
      grains = loop grid (500,0)
  -- part 1
  let [p1@(i,_)] = take 1 $ filter snd $ fmap (isOverflow hMax) <$> zip [1..] grains
  print (i-1)
  -- part 2
  let [p2@(j,_)] = take 1 $ filter snd $ fmap isPeak <$> zip [1..] grains 
  -- putStrLn $ intercalate "\n" $ transpose $ chunksOf 12 $ elems $ grains !! j
  print j
  where
    r = read @Int
    f (x:y:_) = (r x, r y)
    expand :: [Pos] -> [Pos]
    expand [] = []
    expand [x] = [x]
    expand ((x,y):(x',y'):ps)
      | x' > x = (x,y) : expand ((x+1,y):(x',y'):ps) 
      | y' > y = (x,y) : expand ((x,y+1):(x',y'):ps) 
      | x' < x = (x,y) : expand ((x-1,y):(x',y'):ps) 
      | y' < y = (x,y) : expand ((x,y-1):(x',y'):ps) 
      | otherwise = expand ((x',y'):ps)
    isOverflow :: Int -> Grid -> Bool
    isOverflow h = any (\((_,y),c) -> y > h && c=='o') . assocs
    isPeak :: Grid -> Bool
    isPeak g = fromJust (g `at` (500,0)) == 'o'

{-# inline  at #-}
at :: Grid -> Pos -> Maybe Char
at a p
  | inRange (bounds a) p = Just $ a ! p
  | otherwise = Nothing

loop grid pos = let newGrid = sandy grid pos in newGrid : loop newGrid pos

sandy :: Grid -> Pos -> Grid
sandy grid pos = case moveSand grid pos of
  Nothing -> accum (\_ x -> x) grid [(pos, 'o')]
  Just nextPos -> sandy grid nextPos

moveSand :: Grid -> Pos -> Maybe Pos
moveSand grid pos@(x,y) = move under <|> move left <|> move right
  where
    under = (x,y+1)
    left = (x-1,y+1)
    right = (x+1,y+1)
    move p = do
      c <- grid `at` p
      guard (c=='.')
      return p

