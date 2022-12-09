{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Control.Monad.Trans.State as S
import Data.List (nub)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text

data Direction = U | D | L | R deriving (Show)
data Movement = Mov Direction Int deriving (Show)
type Pos = (Int,Int)

main = do
  input <- readFile "data/day9/day9.txt"
  case parse parser "day9" (T.pack input) of
      Left err -> print err
      Right v -> do
        let fs = followers $ steps (0,0) (expand v)
        print $ length . nub $ fs !! 0
        print $ length . nub $ fs !! 8
  where
    parser = many1 parseDirection

expand :: [Movement] -> [Movement]
expand [] = []
expand (Mov d n:ms) = replicate n (Mov d 1) ++ expand ms

move :: Movement -> Pos -> Pos
move (Mov U n) (y,x) = (y+n,x)
move (Mov D n) (y,x) = (y-n,x)
move (Mov L n) (y,x) = (y,x-n)
move (Mov R n) (y,x) = (y,x+n)

adj :: Pos -> Pos -> Bool
adj (x,y) (j,k)
  | ( abs (x-j) <= 1) && (abs (y-k) <= 1) = True
  | otherwise = False

follow :: Pos -> Pos -> Pos
follow t@(ty,tx) h@(hy,hx)
  | adj h t = t
  | hy == ty && hx > tx  = (hy, tx+1)
  | hy == ty && tx > hx = (hy, tx-1)
  | hx == tx && hy > ty = (ty+1, hx)
  | hx == tx && ty > hy = (ty-1, hx)
  | otherwise = let p = align h t in p
  where
    align h (ty,tx) = let
      ne = (ty-1, tx+1)
      nw = (ty-1, tx-1)
      se = (ty+1, tx+1)
      sw = (ty+1, tx-1)
     in head $ filter (adj h) [ne,nw,se,sw]

steps :: Pos -> [Movement] -> [Pos]
steps pos mov = pos : go pos mov
  where
    go _ [] = []
    go p (m:ms) = let np = move m p in np : go np ms

followers xs = let fs = follower xs in fs : followers fs
  where
    follower = scanl follow (0,0)

parseDirection :: Parser Movement
parseDirection = do
  c <- oneOf ['U','D','L','R']
  char ' '
  d <- (read @Int) <$> many1 digit
  optional newline
  return $ Mov (f c) d
    where
      f 'U' = U
      f 'D' = D
      f 'L' = L
      f 'R' = R

