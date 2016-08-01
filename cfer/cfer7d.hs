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
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap
-- import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
-- import qualified Data.Sequence as S

myread = read :: String -> Int
main = do
  n <- readLn
  putList $ solve n

putList :: Show a => [a] -> IO ()
putList = putStrLn . unwords . map show

solve :: Int -> [Int]
solve n = snd . head . sortBy (compare `on` fst) $ zip (map s xs) xs where 
  xs = permutations $ [1..n] ++ [1..n]
  s :: [Int] -> [Int]
  s ss = zipWith (\i x -> (n-i)*abs(d i+i-n)) [1..2*n] ss where
    d i = abs(sum ys) where 
      ys = fst . unzip . filter ((==i).snd) $ zip [1..2*n] ss