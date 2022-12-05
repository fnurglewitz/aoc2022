{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Trans.State.Strict
import Deque.Strict
import Data.List (transpose)
import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified GHC.Exts as E
import Text.InterpolatedString.QM (qms)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text
import Text.ParserCombinators.Parsec.Error

data Crate = 
    None 
  | Crate Char 
  deriving (Eq,Show)

data Command = Cmd Int Int Int

instance Show Command where
  show (Cmd qty from to) = [qms|Move {qty} from {from} to {to}|]

data Ex = Ex {
   crates :: Stacks
 , commands :: [Command]
 } deriving (Show)

type Stacks = Map Int (Deque Crate)

main = do
  input <- readFile "data/day5/day5.txt"
  case parse parser "day5" (T.pack input) of
      Left err -> print err
      Right (Ex crates commands) -> do
          print $ fmap (unCrate . fromJust . snd) $  M.toList $ Deque.Strict.head <$> foldl (applyCommand unconsN) crates commands
          print $ fmap (unCrate . fromJust . snd) $  M.toList $ Deque.Strict.head <$> foldl (applyCommand unconsN') crates commands
  where
    parser = do
      crates <- toStacksMap . fmap (Prelude.filter (/=None)) . transpose <$> parseCrates
      parseIndexes
      Ex crates <$> parseCommands
    unCrate :: Crate -> Char
    unCrate (Crate c) = c

toStacksMap :: [[Crate]] -> Stacks
toStacksMap cs = fmap toDeque $ M.fromList $ zip [1..] cs
  where
    toDeque :: [Crate] -> Deque Crate
    toDeque [] = mempty
    toDeque (x:xs) = cons x (toDeque xs)

applyCommand :: (Int -> Deque Crate -> Maybe ([Crate], Deque Crate)) -> Stacks -> Command -> Stacks
applyCommand unconz stacks (Cmd num from to) = flip execState stacks $ do
  dqFrom <- gets (M.! from) 
  dqTo <- gets (M.! to) 
  let (elems, dqFrom') = fromJust $ unconz num dqFrom
      dqTo' = consN elems dqTo
  modify (M.insert from dqFrom') 
  modify (M.insert to dqTo')

unconsN :: Int -> Deque a -> Maybe ([a], Deque a)
unconsN 0 d = Nothing
unconsN n d = go n d []
  where
    go 0 d acc = Just (acc, d)
    go n d acc = case Deque.Strict.uncons d of
                   Nothing -> Nothing
                   Just (elem, newD) -> go (n-1) newD (elem : acc)

unconsN' :: Int -> Deque a -> Maybe ([a], Deque a)
unconsN' 0 d = Nothing
unconsN' n d = go n d []
  where
    go 0 d acc = Just (acc, d)
    go n d acc = case Deque.Strict.uncons d of
                   Nothing -> Nothing
                   Just (elem, newD) -> go (n-1) newD (acc ++ [elem])

consN :: [a] -> Deque a -> Deque a
consN xs d = foldr Deque.Strict.cons d xs

parseCrate :: Parser Crate
parseCrate = do
  try parsePresent <|> try parseMissing
  where
    parsePresent :: Parser Crate
    parsePresent = do
      char '['
      c <- letter
      char ']'
      optional $ char ' '
      return $ Crate c
    parseMissing :: Parser Crate
    parseMissing = do
      string "   "
      optional $ char ' '
      return None

parseCrates :: Parser [[Crate]]
parseCrates = do
  many1 parseLineOfCrates
  where
    parseLineOfCrates = many1 parseCrate >>= \crates -> newline >> return crates

parseIndexes :: Parser ()
parseIndexes = do
  many $ noneOf "\n" -- just ignore them
  many1 newline
  return ()

parseCommand :: Parser Command
parseCommand = do
  string "move "
  qty <- (read @Int) <$> many1 digit
  string " from "
  from <- (read @Int) <$> many1 digit
  string " to "
  to <- (read @Int) <$> many1 digit
  return $ Cmd qty from to

parseCommands :: Parser [Command]
parseCommands = do
  many1 $ do
    cmd <- parseCommand
    optional newline
    return cmd
