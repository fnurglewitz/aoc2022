{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.List (notElem)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text

data Op = NoOp | AddX Int deriving(Show)
data Reg = Reg { x :: Int } deriving (Show)
type Screen = [(Int,Char)]

main = do
  input <- readFile "data/day10/day10.txt"
  case parse parser "day10" (T.pack input) of
      Left err -> print err
      Right v -> do
        let cycles = fun (Reg 1) v
        print $ sum $ fmap fromJust $ flip lookup (ss <$> cycles) <$> [20,60,100,140,180,220] 
        putStrLn . Main.crlf $ snd <$> draw cycles screen
  where
    parser = many1 parseOp
    ss (c, (Reg x,_)) = (c, c*x)

screen :: Screen
screen = (,'.') <$> [1..40*6]

draw :: [(Int,(Reg,Reg))] -> Screen -> Screen
draw [] [] = []
draw ((c,(r,_)):rs) (s:ss) = updatePixel c r s : draw rs ss

updatePixel :: Int -> Reg -> (Int,Char) -> (Int,Char)
updatePixel c (Reg r) p@(ix, chr) 
  | ((c-1) `mod` 40) `notElem` [r-1..r+1] = p
  | otherwise = (ix, '#')

crlf :: String -> String
crlf xs = go 0 xs
  where 
    go _ [] = []
    go n (x:xs)
      | n `mod` 40 == 0 = '\n' : x : go (n+1) xs
      | otherwise = x : go (n+1) xs

fun :: Reg -> [Op] -> [(Int,(Reg,Reg))]
fun _ [] = []
fun r os = go r os 0
  where
    go r [] acc = []
    go r (NoOp:os) acc = (acc+1,(r,r)) : go r os (acc+1)
    go r@(Reg x) (AddX n:os) acc = let r' = Reg (x+n) in (acc+1,(r,r)) : (acc+2,(r,r')) : go r' os (acc+2)

parseOp :: Parser Op
parseOp = do
  op <- parseNoOp <|> parseAddX
  optional newline
  return op
  where
    parseNoOp = string "noop" >> return NoOp
    parseAddX = do
      string "addx "
      sign <- maybe 1 (const (-1)) <$> optionMaybe (char '-')
      n <- (read @Int) <$> many1 digit
      return $ AddX (sign*n)


