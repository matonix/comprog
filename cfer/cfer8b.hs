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
import           Data.Array.Unboxed
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as C
-- import           Data.ByteString.Char8 ()
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap
-- import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
-- import qualified Data.Sequence as S

myread = fromIntegral::Int -> Int64
main = do
  s <- map (myread . digitToInt) <$> getLine
  print $ solve s 

solve :: [Int64] -> Int64
solve xs = go (reverse xs) 0 (pred . myread $! length xs) where
  go [x] v _ = v + divisible x
  go (x:y:xs) v l = go (y:xs) (v + divisible x + if (x+10*y)`mod`4==0 then l else 0::Int64) (pred l)
  divisible x = if x`mod`4==0 then 1::Int64 else 0::Int64