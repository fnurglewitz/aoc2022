{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad.Trans.State.Strict as S
import Data.List (partition, sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text

main = do
  input <- readFile "data/day7/day7.txt"
  case parse parser "day7" (T.pack input) of
      Left err -> print err
      Right v -> do
          let 
            fileSystem = applyCommands root v
            totalSize = size fileSystem
            targetSize = abs $ 70000000 - 30000000 - totalSize
            dirs = directories fileSystem
          print $ sum $ filter (<100000) $ size <$> dirs
          print $ head $ fmap snd $ sortBy (\(a,_) (b,_) -> b `compare` a ) $ filter ((<0) . fst) $ fmap (f targetSize . size) dirs
  where
    parser = skip >> many1 parseCommand
    skip = string "$ cd /" >> newline
    root = Directory "/" []
    f t s = (t-s,s)

data Command = CD Text | LS [FileSystemEntry] deriving (Eq,Show)

data FileSystemEntry = 
      Directory Text [FileSystemEntry] 
    | File Text Int
    deriving (Eq,Show)

size :: FileSystemEntry -> Int
size (File _ i) = i
size (Directory _ xs) = sum (size <$> xs)

findDir s (Directory n _) = n == s
findDir _ _ = False

applyCommands :: FileSystemEntry -> [Command] -> FileSystemEntry
applyCommands fs cmds = flip S.evalState ([] :: [Text]) $ go fs cmds
  where
    go fs [] = return fs
    go fs (c:cs) = evalCmd fs c >>= flip go cs
    evalCmd fs (CD "..") = do
      S.modify init
      return fs
    evalCmd fs (CD dir) = do
      S.modify (++[dir])
      return fs
    evalCmd fs lss = do
      cwd <- S.get
      return $ updateFS fs cwd lss

updateFS :: FileSystemEntry -> [Text] -> Command -> FileSystemEntry
updateFS (Directory dn _) [] (LS ls) = Directory dn ls
updateFS (Directory dn ds) cwd@(x:xs) ls =
  let
    (td:_,rest) = partition (findDir x) ds
    updated = updateFS td xs ls
  in Directory dn (updated : rest)

directories :: FileSystemEntry -> [FileSystemEntry]
directories dir@(Directory n ds) = dir : concatMap directories ds
directories _ = []

parseCommand = string "$ " >> (parseCD <|> parseLS)

parseCD :: Parser Command
parseCD = do
  string "cd "
  dirName <- manyTill anyChar newline
  return . CD . T.pack $ dirName

parseLS :: Parser Command
parseLS = do
  string "ls"
  newline
  infos <- many1 (parseFileInfo <|> parseDirInfo)
  return . LS $ infos
  where
    parseFileInfo :: Parser FileSystemEntry
    parseFileInfo = do
      size <- (read @Int) <$> many1 digit
      char ' '
      fileName <- manyTill anyChar newline
      return $ File (T.pack fileName) size
    parseDirInfo :: Parser FileSystemEntry
    parseDirInfo = do
      string "dir "
      dirName <- manyTill anyChar newline
      return $ Directory (T.pack dirName) []

