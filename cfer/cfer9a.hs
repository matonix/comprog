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
  [n,p] <- map myread.words <$> getLine
  ns <- replicateM n getLine
  print $ solve (toint64 p) ns

solve :: Int64 -> [String] -> Int64
solve p ns = p`div`2 * go 0 0 (map ((==4).length) $ reverse ns) where
  go :: Int64 -> Int64 -> [Bool] -> Int64
  go v w [] = w
  go v w (True:xs) = go (v*2) (w+(v*2)) xs
  go v w (False:xs) = go (v*2+1) (w+(v*2+1)) xs