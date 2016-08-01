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
import           Text.Printf
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
  [hh,mm] <- map myread . (\xs -> [take 2 xs, drop 3 xs]) <$> getLine
  m <- readLn
  solve hh mm m

solve :: PrintfType r => Int -> Int -> Int -> r
solve hh mm m = printf "%02d:%02d\n" nh nm where
  nh = mod (hh+(div (mm+m) 60)) 24
  nm = mod (mm+m) 60