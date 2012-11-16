module Statistics.Types.Debug where

import System.IO
import qualified Data.Traversable as Tv


class CSV_Output a where
  toCSV :: a -> String  

dumpCSV :: (CSV_Output a, Tv.Traversable t) => t a -> FilePath -> IO ()
dumpCSV cs fp =
  do h <- openFile fp WriteMode
     Tv.mapM (\c -> hPutStrLn h (toCSV c)) cs
     hClose h

dumpCSV2 :: (CSV_Output a) => [a] -> FilePath -> IO ()
dumpCSV2 cs fp =
  do h <- openFile fp WriteMode
     mapM_ (\c -> hPutStrLn h (toCSV c)) cs
     hClose h
