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

volume !(r,h) = r*r*h

myread = read :: String -> Int64
main = do
  n <- readLn
  ns <- map ((\[r,h]->(myread r,myread h)).words) <$> replicateM n getLine
  print $ solve n ns

solve :: Int -> [(Int64,Int64)] -> Double
solve n ns = pi * (fromIntegral . ans . reverse . map volume $ ns) where
  ans [x] = x
  ans [x2,x1] = if x2>x1 then x2+x1 else x1
  ans (xi:xj:xs) = if xi>xj then xi + ans (xj:xs) else max xi (ans (xj:xs))