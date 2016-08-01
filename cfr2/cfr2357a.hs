{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE TupleSections #-}

import           System.IO hiding (char8)
import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Tuple
import           Data.Int
import           Data.Char
import           Data.Function (on)
import           Data.Array
-- import           Data.Array.Unboxed
-- import           Data.Array.IArray
-- import           Data.Array.ST
-- import           Data.Array.MArray
-- import           Data.Array.Unsafe
import           Data.Ix
import           Data.Maybe
import           Data.Monoid hiding ((<>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.ByteString.Builder
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntSet (IntSet)
-- import qualified Data.IntSet as S
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as M
-- import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
-- import qualified Data.Sequence as S
-- import           Debug.Trace

main = do
  n <- readInt1 <$> BS.getLine
  ns <- map (readStrIntInt . toTriple . BS.words) <$> replicateM n BS.getLine
  putStrLn $ solve ns

readStrIntInt :: (BS.ByteString, BS.ByteString, BS.ByteString) -> (BS.ByteString, Int, Int)
readStrIntInt = applyTriple id (fst . fromJust . BS.readInt) (fst . fromJust . BS.readInt)

solve :: [(BS.ByteString,Int,Int)] -> String
solve ns = if any p ns then "YES" else "NO" where
  p = \(_,before,after) -> before>=2400 && before<after

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  

readInt1 :: BS.ByteString -> Int
readInt1 = fst . fromJust . BS.readInt 

readInt2 :: BS.ByteString -> (Int,Int)
readInt2 = toTuple . readIntN

readInt3 :: BS.ByteString -> (Int,Int,Int)
readInt3 = toTriple . readIntN

readIntN :: BS.ByteString -> [Int]
readIntN =  map readInt1 . BS.words

readInt641 :: BS.ByteString -> Int64
readInt641 = fromIntegral . fst . fromJust . BS.readInteger

readInt642 :: BS.ByteString -> (Int64,Int64)
readInt642 = toTuple . readInt64N

readInt643 :: BS.ByteString -> (Int64,Int64,Int64)
readInt643 = toTriple . readInt64N

readInt64N :: BS.ByteString -> [Int64]
readInt64N =  map readInt641 . BS.words

readInteger1 :: BS.ByteString -> Integer
readInteger1 = fst . fromJust . BS.readInteger 

readInteger2 :: BS.ByteString -> (Integer,Integer)
readInteger2 = toTuple . readIntegerN

readInteger3 :: BS.ByteString -> (Integer,Integer,Integer)
readInteger3 = toTriple . readIntegerN

readIntegerN :: BS.ByteString -> [Integer]
readIntegerN =  map readInteger1 . BS.words

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)

toTriple :: [a] -> (a, a, a)
toTriple [x, y, z] =(x, y, z)

fromTuple :: (a, a) -> [a]
fromTuple (x, y) = [x, y]

fromTriple :: (a, a, a) -> [a]
fromTriple (x, y, z) = [x, y, z]

-- if not applying, use "const"

applyTuple :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
applyTuple f g (x, y) = (f x, g y)

applyTriple :: (a -> a') -> (b -> b') -> (c -> c') -> (a, b, c) -> (a', b', c')
applyTriple f g h (x, y, z) = (f x, g y, h z)