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
  [a,b,c] <- map myread.words <$> getLine
  print $ solve a b c 

solve :: Int -> Int -> Int -> Double
solve a b c
  | b + c < a = circle (a+b+c) - circle (a-b-c)
  | a + c < b = circle (a+b+c) - circle (b-a-c)
  | a + b < c = circle (a+b+c) - circle (c-a-b)
  | otherwise = circle (a+b+c)

circle :: Int -> Double
circle !r = pi*fromIntegral (r*r)