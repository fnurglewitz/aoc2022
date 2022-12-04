{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (intersect)
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
          print $ length $ filter (==True) $ fmap (shitCheck contains) $ fmap range <$> x
          print $ length $ filter (==True) $ fmap (shitCheck overlaps) $ fmap range <$> x
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

range :: Range -> [Int]
range (x,y) = [x..y]

contains :: Eq a => [a] -> [a] -> Bool
contains xs ys = let
    xLen = length xs
    yLen = length ys
    intersectionLen = length $ xs `intersect` ys
   in  intersectionLen == xLen || intersectionLen == yLen

overlaps :: Eq a => [a] -> [a] -> Bool
overlaps xs ys = not $ null (xs `intersect` ys)

shitCheck :: Eq a => ([a] -> [a] -> Bool) -> [[a]] -> Bool
shitCheck f (xs:ys:_) = f xs ys
