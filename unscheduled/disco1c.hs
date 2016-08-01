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
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as C
-- import           Data.ByteString.Char8 ()
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap

myread = read :: String -> Int
main = do
  s <- getLine
  k <- readLn
  print $ solve s k

solve :: String -> Int -> String
solve s k = go s k (nub $ sort s)
  where
    go s 0 _ = s
    go s k a = go (minimum [delete s,replace s a,insert s]) (k-1) a
      where
        delete [] = []
        delete s = []
        delete s:t:ss = 
