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

bigmod = (`mod` 1000000007)

myread = read :: String -> Int
main = do
  n <- readLn
  ns <- replicateM n readLn
  print $ solve ns

solve :: [Int] -> Int
solve ns = 0

palindrome :: [a] -> Int -> [[a]]
palindrome as 0 = [[]]
palindrome as 1 = map (\x -> [x]) as 
palindrome as n = half as (n`div`2)