{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text
import Text.ParserCombinators.Parsec.Error

type Range = (Int,Int)

main = do
  input <- readFile "data/day4/day4.txt"
  case parse parser "day4" (T.pack input) of
      Left err -> print err
      Right x -> do
          print $ length $ filter (==True) $ shitCheck contains <$> x
          print $ length $ filter (==True) $ shitCheck overlaps <$> x
  where
      parser = many1 parseLine

parseInt :: Parser Int
parseInt = do
  num <- T.pack <$> many1 digit
  case TR.decimal num of
    Right n -> return (fst n)
    Left _ -> error "parseInt: should not be here"

parseRange :: Parser Range
parseRange = do
  i <- parseInt
  char '-'
  j <- parseInt
  return (i,j)

parseLine :: Parser [Range]
parseLine = do
  r1 <- parseRange
  char ','
  r2 <- parseRange
  optional newline
  return [r1,r2]

contains :: Range -> Range -> Bool
contains (x,y) (x',y') = (x >= x' && y <= y') || (x' >= x && y' <= y)

overlaps :: Range -> Range -> Bool
overlaps (x,y) (x',y') = not (y' < x || x' > y)

shitCheck :: (Range -> Range -> Bool) -> [Range] -> Bool
shitCheck f (xs:ys:_) = f xs ys
