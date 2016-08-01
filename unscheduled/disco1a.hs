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
  n <- readLn
  mapM_ putStrLn (solve n)

solve :: Int -> [String]
solve n = go n "DiscoPresentsDiscoveryChannelProgrammingContest2016"
  where
    go n [] = []
    go n xs = (take n xs):(go n (drop n xs))