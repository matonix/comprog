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
import           Data.Array
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
  [n,m,k] <- map myread.words <$> getLine
  qs <- map ((\[q,i,c]->(q,i,c)) . map myread . words) <$> replicateM k getLine
  putMatrix m $ solve n m (listArray ((1,1),(n,m)) $ repeat 0) qs

putMatrix n = mapM_ putStrLn . map (unwords . map show) . cut n . elems
cut n [] = []
cut n xs = take n xs:cut n (drop n xs)

putList :: Show a => [a] -> IO ()
putList = putStrLn . unwords . map show

solve :: Int -> Int -> Array (Int,Int) Int -> [(Int,Int,Int)] -> Array (Int,Int) Int
solve n m as qs = update as $ reduce qs where
  update as [] = as
  update as ((1,i,c):qs) = update (as//[((i,j),c) | j <- [1..m]]) qs
  update as ((2,j,c):qs) = update (as//[((i,j),c) | i <- [1..n]]) qs
  reduce qs = reverse $ go (reverse qs) (listArray ((1,1),(2,max n m)) $ repeat False) where
    go [] _ = []
    go ((q,i,c):qs) done = if done!(q,i)
      then go qs done
      else (q,i,c):go qs (done//[((q,i),True)])