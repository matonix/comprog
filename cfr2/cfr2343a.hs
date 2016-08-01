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
-- import           Data.Array
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as C
-- import           Data.ByteString.Char8 ()
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap
-- import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
-- import qualified Data.Sequence as S

myread = read :: String -> Int
main = do
  n <- readLn
  nss <- replicateM n getLine
  print $ solve nss (transpose nss)

solve :: [String] -> [String] -> Int
solve nss mss = (sum (map count nss)) + (sum (map count mss)) where
  count :: String -> Int
  count = comb2 . length . filter (=='C')

comb2 :: Int -> Int
comb2 1 = 0
comb2 0 = 0
comb2 n = n*(n-1)`div`2