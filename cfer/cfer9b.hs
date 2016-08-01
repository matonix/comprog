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
toint64 = fromIntegral :: Int -> Int64
main = do
  n <- readLn
  ps <- map (toint64.myread) . words <$> getLine
  cs <- getLine
  print $ solve n ps cs

-- solve :: Int -> [Int64] -> [Char] -> Int64
-- solve n ps cs = maximum . map (score . zip ps) $ flips cs where
--   flips :: [Char] -> [[Char]]
--   flips cs = 
  -- flipAt :: [Int64] -> [Char] -> Int -> [(Int64,Char)]
  -- flipAt ps cs n = zip ps cs' where
  --   cs' = map f (take n cs) ++ drop n cs where
  --     f 'A' = 'B'
  --     f 'B' = 'A'
  -- score :: [(Int64,Char)] -> Int64
  -- score = (\(a,b) -> max (f a) (f b)) . partition ((=='A').snd) where
  --   f = sum.fst.unzip

solve :: Int -> [Int64] -> [Char] -> Int64
solve n ps cs = go (0,0) (score 'A',score 'B') (max (score 'A') (score 'B')) ps cs where
  score c = sum . fst . unzip . filter ((==c).snd) $ zip ps cs
  go (pa,pb) (sa,sb) m [] [] = m
  go (pa,pb) (sa,sb) m (p:ps) ('A':cs) = go (pa,pb+p) (sa-p,sb) (maximum [m,pa+sa-p,pb+sb+p]) ps cs
  go (pa,pb) (sa,sb) m (p:ps) ('B':cs) = go (pa+p,pb) (sa,sb-p) (maximum [m,pa+sa+p,pb+sb-p]) ps cs