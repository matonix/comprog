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
  [n,k] <- map myread.words <$> getLine
  xs <- getLine
  putStrLn $ solve n k xs

solve :: Int -> Int -> String -> String
solve n k xs = go k xs [] where
  go 0 xs as = (reverse as) ++ xs
  go _ [] _ = "-1"
  -- go k (x:xs) as = go 

dist :: String -> String -> Int
dist [x] [y] = abs (ord x - ord y)
dist (x:xs) (y:ys) = dist xs ys + abs (ord x - ord y)

tsid :: Char -> Int -> (Char,Int)
tsid x d
  | ord x < ord 'n' && ord 'z' - ord x >= d = (chr (ord x+d),d)
  | ord x < ord 'n' && ord 'z' - ord x < d  = ('z',ord 'z' - ord x)
  | ord x > ord 'm' && ord x - ord 'a' >= d = (chr (ord x-d),d)
  | otherwise                               = ('a',ord x - ord 'a')