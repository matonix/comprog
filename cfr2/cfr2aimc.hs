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
import           Data.Set (Set)
import qualified Data.Set as Set
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
  [n,m] <- map myread.words <$> getLine
  ms <- map (Set.fromList . map myread . words) <$> replicateM m getLine
  printer $ solve n ms

printer :: String -> IO ()
printer [] = putStrLn "No"
printer xs = do
  putStrLn "Yes"
  putStrLn xs

solve :: Int -> [Set Int] -> String
solve n ms = if p n ms
  then
    let a = filter null $ map (go n ms) "abc" in
    if null a then [] else head a
  else []
    where
      p n ms = (<3) . maximum $ map (count ms) [1..n] where
        count ms x = length $ filter (Set.member x) ms
      go :: Int -> [Set Int] -> Char -> String
      go n ms c = goo 1 n ms (take n ("c":(repeat "abc"))) where
        goo s n ms xs = if s == n
          then if any null xs then [] else filter head xs
          else goo (s+1) n ms ()
