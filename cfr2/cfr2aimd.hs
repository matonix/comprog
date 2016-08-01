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
  [n,a,b] <- map myread.words <$> getLine
  xs <- map myread.words <$> getLine
  print $ solve n a b xs

solve :: Int -> Int -> Int -> [Int] -> Int
solve =