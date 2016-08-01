import           Control.Applicative
import           Control.Monad
import           Control.Arrow
import           Data.List
import           Data.Tuple
import           Data.Int
import           Data.Char
import           Data.Function
import           Data.Ratio
import           Data.Array
import           Data.Bits

modG7 x = mod x 1000000007

-- SICP Power iteration
-- 結合則のある二項演算 -> 基数部 -> 指数部 -> 単位元
power :: Integral a => (t -> t -> t) -> t -> a -> t -> t
power _ _ 0 e = e
power f a n e = power f (f a a) (div n 2) (if odd n then f a e else e)

-- n^k = exponentialfunction n k (same speed)
expo n k = power (*) n k 1

fib3 :: Int -> Integer
fib3 0 = 0
fib3 n = truncate . (* 2) . snd $ power f (1 % 2, 1 % 2) n (1, 0)
  where f (a1, b1) (a2, b2) = (a1 * a2 + 5 * b1 * b2, a1 * b2 + a2 * b1)

-- gosper and alamin's idea
-- http://tsumuji.cocolog-nifty.com/tsumuji/2013/11/post-d6f6.html
fib2 :: Int -> Integer
fib2 n = fst $ power gsMulti (1, 0) n (0, 1) where
  gsMulti (a, b) (c, d) = (a * (c + d) + b * c, a * c + b * d)

-- An even faster version, given later by wli on the IRC channel.
-- https://wiki.haskell.org/The_Fibonacci_sequence
-- need to import Data.Bits & List

fib :: Int -> Integer
fib n = snd . foldl' fib' (1, 0) . dropWhile not $
            [testBit n k | k <- let s = finiteBitSize n in [s-1,s-2..0]]
    where
        fib' (f, g) p
            | p         = (f*(f+2*g), ss)
            | otherwise = (ss, g*(2*f-g))
            where ss = f*f+g*g

-- http://tsumuji.cocolog-nifty.com/tsumuji/2011/02/haskell-0b4a.html
type Vector a = [a]
type Matrix a = [Vector a]

-- Vector * Vector (inner product)
(/*/) :: Num a => Vector a -> Vector a -> a
(/*/) xs ys = sum $ zipWith (*) xs ys
infixl 7 /*/

-- Matrix * Matrix
(|*|) :: Num a => Matrix a -> Matrix a -> Matrix a
(|*|) xss yss = [[xs /*/ ys | ys <- yss'] | xs <- xss]
    where yss' = transpose yss
infixl 7 |*|

-- lucas :: Int -> Integer
-- lucas 0 = 2
-- lucas 1 = 1
-- lucas n = fst4 $ power f (1,1,1,0) (n-2) (1,0,0,1) `f` (1,2,2,-1) where
--   fst4 (a,_,_,_) = a
--   f (a1, b1, c1, d1) (a2, b2, c2, d2) = (a', b', c', d') where
--     a' = a1 * a2 + b1 * c2
--     b' = a1 * b2 + b1 * d2
--     c' = c1 * a2 + d1 * c2
--     d' = c1 * b2 + d1 * d2

lucas :: Int -> Integer
lucas 0 = 2
lucas 1 = 1
lucas n = fst3 $ power f (1,1,0) (n-2) (1,0,1) `f` (1,2,-1) where
  fst3 (a,_,_) = a
  f (a1, b1, d1) (a2, b2, d2) = (a', b', d') where
    a' = a1 * a2 + b1 * b2
    b' = a1 * b2 + b1 * d2
    d' = b1 * b2 + d1 * d2

-- http://qiita.com/myuon_myon/items/ad006568bd187223f494#2-4
primes = 2:3:[x|i<-[1..], j<-[-1,1], let x = 6*i+j, isPrime x] where
  isPrime n = null [i|i<-takeWhile (\x -> x*x <= n) primes, rem n i == 0]