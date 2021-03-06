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

main :: IO ()
main = BS.getLine >>= print . solve . readInt64N

solve :: [Int64] -> Int64
solve [h,w,a,b] = sum' $ map (\k -> bino (k+b-2) (b-1) *% bino (h-k+w-b-1) (w-b-1)) [1..h-a] where
  facts :: Array Int64 Int64
  facts = listArray (0,h+w-2) $ map fact [0..h+w-2] where
    fact 0 = 1
    fact n = n*%facts!(n-1)
  bino :: Int64 -> Int64 -> Int64
  bino n k = facts!n /% (facts!k *% facts!(n-k))
  sum' :: [Int64] -> Int64
  sum' [] = 0
  sum' (x:xs) = x +% sum xs

-- solve [h,w,a,b] = go (1,1) where
--   memo = listArray ((1,1),(h,w)) $ map go $ range ((1,1),(h,w))
--   go (y,x)
--     | y==h && x==w = 1::Int64
--     | y+a>h && x<=b = 0
--     | y>h = 0
--     | x>w = 0
--     | otherwise = memo!(y+1,x) +% memo!(y,x+1)

-- bino :: Int64 -> Int64 -> Int64
-- bino n k = bino' n (if k*2 <= n then k else n-k) where
--   bino' _ 0 = 1
--   bino' 0 _ = 0
--   bino' n k = bino (n-1) (k-1) *% n /% k

-- mod modules --

modNum = 1000000007

modd a = a `mod` modNum

(+%) a b = modd (modd a + modd b)
infixl 6 +%

(-%) a b = modd (modd a - modd b + modNum)
infixl 6 -%

-- usable on Int64
(*%) a b = modd (modd a * modd b)
infixl 7 *%

-- usable only if modNum is prime
(/%) a b = a *% powerMod b (modNum-2) modNum where
  -- SICP Power iteration
  power :: Integral a => (t -> t -> t) -> t -> a -> t -> t
  power _ _ 0 e = e
  power f a n e = power f (f a a) (div n 2) (if odd n then f a e else e)
  powerMod a n m = power (*%) a n 1
infixl 7 /%

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
