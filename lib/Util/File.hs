module Util.File where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

readLines :: FilePath -> IO [String]
readLines path = fmap T.unpack <$> (T.lines <$> TIO.readFile path)
