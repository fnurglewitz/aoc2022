{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (foldM)
import qualified Control.Monad.Trans.State.Lazy as S
import Data.List (partition, sortBy)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.InterpolatedString.QM (qms)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text

type Operation = (Integer -> Integer)
type Test = (Integer -> Integer)

data Monkey = Monkey {
  ix :: Integer,
  items :: [Integer],
  op :: Operation,
  test :: (Test,Integer),
  counter :: Integer
}

instance Show Monkey where
  show (Monkey ix is _ (_,t) c) = [qms|{ix}: {is} - {t} - {c}|] 

type Monkeys = M.Map Integer Monkey

main = do
  input <- readFile "data/day11/day11.txt"
  case parse parser "day11" (T.pack input) of
      Left err -> print err
      Right v -> do
        let monkeys = M.fromList v
        print $ (\[x,y] -> x*y) $ take 2 $ sortBy (flip compare) $ fmap counter . M.elems $ S.evalState (day11 (`div` 3)) monkeys !! 19
        let pr = foldl1 lcm $ snd . test <$> M.elems monkeys
        print $ (\[x,y] -> x*y) $ take 2 $ sortBy (flip compare) $ fmap counter . M.elems $ S.evalState (day11 (`mod` pr)) monkeys !! 9999
  where
    parser = many1 $ do
      m <- parseMonkey
      optional newline 
      optional newline 
      return m

day11 :: (Integer -> Integer) -> S.State Monkeys [Monkeys]
day11 md = do
  ks <- M.keys <$> S.get
  go ks
  ms <- S.get
  (ms:) <$> day11 md
  where
    go [] = return ()
    go (k:ks) = do
      m <- S.gets (M.! k)
      monke md m >> go ks

monke :: (Integer->Integer) -> Monkey -> S.State Monkeys ()
monke md (Monkey _ [] _ _ _) = return ()
monke md m@(Monkey ix (i:is) op t@(tst,_) cnt) = do
  ms <- S.get
  let
   newWl = md $ op i
   dst = tst newWl
   mDst = ms M.! dst
  S.modify $ M.insert ix $ m { ix = ix, items = is, op = op, test = t, counter=cnt+1 }
  S.modify $ M.insert dst $ monkeReceive newWl mDst
  S.get >>= \ms -> monke md $ ms M.! ix
  where
    monkeReceive n (Monkey ix is op tst cnt) = Monkey ix (is ++ [n]) op tst cnt

parseMonkey :: Parser (Integer, Monkey)
parseMonkey = do
  string "Monkey "
  mn <- (read @Integer) <$> many digit
  char ':'
  newline
  si <- parseStartingItems
  newline
  op <- parseOperation
  newline
  tst <- parseTst
  return (mn, Monkey mn si op tst 0)

parseStartingItems :: Parser [Integer]
parseStartingItems = do
  string "  Starting items: "
  nums
  where
    nums = many1 $ do
      n <- (read @Integer) <$> many1 digit
      optional (string ", ")
      return n

parseOperation :: Parser Operation
parseOperation = do
  string "  Operation: "
  try sumByConst <|> try mulByConst <|> try squareOp
  where
    -- dumb but gets the job done
    mulByConst = string "new = old * " >> (read @Integer) <$> many1 digit >>= \n -> return (*n)
    sumByConst = string "new = old + " >> (read @Integer) <$> many1 digit >>= \n -> return (+n)
    squareOp = string "new = old * old" >> return (^2)

parseTst :: Parser (Test, Integer)
parseTst = do
  string "  Test: divisible by "
  n <- (read @Integer) <$> many1 digit
  newline
  string "    If true: throw to monkey "
  mTrue <- (read @Integer) <$> many1 digit
  newline
  string "    If false: throw to monkey "
  mFalse <- (read @Integer) <$> many1 digit
  return (\x -> if x `mod` n == 0 then mTrue else mFalse,n)

