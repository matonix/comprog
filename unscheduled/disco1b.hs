{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Tuple
import           Data.Int
import           Data.Char
import           Data.Function
import           Debug.Trace
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as C
-- import           Data.ByteString.Char8 ()
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap

myread = read :: String -> Int
main = do
  n <- readLn
  as <- map myread.words <$> getLine
  print $ solve n as

solve :: Int -> [Int] -> Int
solve n as = go n 0 (cycle as) (sort as)
  where
    go n st _ [] = (st-1) `div` n + 1
    go n 0 (c:cy) (s:so) = if c == s
      then go n 1 cy so
      else go n 0 cy (s:so)
    go n m (c:cy) (s:so) = if c == s
      then go n (m+1) cy so
      else go n (m+1) cy (s:so)
