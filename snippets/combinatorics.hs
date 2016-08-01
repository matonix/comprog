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

modG7 x = mod x 1000000007

brute :: [a] -> Int -> [[a]]
brute x n = iterate (liftA2 (:) x) [[]] !! n

brute2 :: [a] -> Int -> [[a]]
brute2 x n = replicateM n x

perm :: Eq a => [a] -> Int -> [[a]]
perm _ 0 = [[]]
perm xs n = [x:ys | x <- xs, ys <- perm (delete x xs) (n-1)]

perm2 :: Eq a => [a] -> Int -> [[a]]
perm2 _ 0 = [[]]
perm2 xs n = [x:ys | x <- xs, ys <- perm2 xs (n-1)]

-- fast (dict)
perm3 :: [a] -> Int -> [[a]]
perm3 [] _    = [[]]
perm3 ns size = perm' size (length ns - 1) [(ns, [])]
    where
      perm' 0 _ xs = [a | (_, a) <- xs]
      perm' c n xs = perm' (c - 1) (n - 1) $ concatMap (f n) xs
      f n (xs, ys) = [(as ++ bs, ys ++ [b]) |
                      x <- [0 .. n], let (as, b : bs) = splitAt x xs]

-- not fast
perm4 :: [a] -> Int -> [[a]]
perm4 _ 0  = [[]]
perm4 xs n = concatMap (\(y, ys) -> map (y:) (perm4 ys (n - 1))) $ select xs
  where
    select :: [a] -> [(a, [a])]
    select [x]    = [(x, [])]
    select (x:xs) = (x, xs) : map (second ((:) x)) (select xs)

comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb _ [] = error "combination empty list"
comb n a@(x:xs)
  | n == length a = [a]
  | otherwise     = [x:y | y <- ys1] ++ ys2
      where
        ys1 = comb (n - 1) xs
        ys2 = comb n xs

comb2 :: Int -> [a] -> [[a]]
comb2 0 _      = [[]]
comb2 _ []     = []
comb2 n (x:xs) = map (x:) (comb2 (n-1) xs) ++ comb2 n xs

comb3 :: Int -> [a] -> [[a]]
comb3 n xs = comb' n xs [] [] where
  comb' 0 _      ys zs = reverse ys : zs
  comb' _ []     _  zs = zs
  comb' n (x:xs) ys zs = comb' (n - 1) xs (x:ys) (comb' n xs ys zs)

comb4 :: [a] -> Int -> [[a]]
comb4 xs n = comb' n [(xs, [])]
    where
      comb' 0 xs = [a | (_, a) <- xs]
      comb' c xs = comb' (c - 1) $ concatMap f xs
      f (xs, ys) = [(as, ys ++ [a]) | (a : as) <- tails xs]

-- fast
comb5 :: [a] -> Int -> [[a]]
comb5 [] _ = [[]]
comb5 ns size = comb' size [(ns, [])]
    where
      comb' 0 xs = [a | (_, a) <- xs]
      comb' c xs = comb' (c - 1) $ concatMap comb'' xs
      comb'' (x : xs, ys) = (xs, ys ++ [x]) : comb'' (xs, ys)
      comb'' _ = []

-- nCk = binomialCoefficent n k
-- bino n r = iterate (scanl1 (+)) [1,1..] !! (n-r) !! r
-- bino2 n 0 = 1
-- bino2 0 _ = 0
-- bino2 n k = bino (n-1) (k-1) + bino (n-1) k
--fast
bino n k = bino' n (if k*2 <= n then k else n-k) where
  bino' _ 0 = 1
  bino' 0 _ = 0
  bino' n k = bino (n-1) (k-1) * n `div` k 

-- nHk = multisetCoefficient n k
multisetCoefficient n k = bino (n+k-1) k

-- Catalan number
catalan n = bino (2*n) n `div` (n+1)

-- fact n = product [1..n]
-- nPk = fallingFactorial n k
-- fallfact n k = product [n-k+1..n]
-- risefact n k = product [n..n+k-1]

-- fact 0 = 1
-- fact n = fact m ^ 2 * bino n m * if even n then 1 else m+1 where m = n`div`2

fact 0 = 1
fact n = fact (n-1) * n

fallfact _ 0 = 1
fallfact n k = fallfact (n-1) (k-1) * n

risefact _ 0 = 1
risefact n k = risefact (n+1) (k-1) * n

-- import Data.Ratio
-- http://tsumuji.cocolog-nifty.com/tsumuji/2013/11/post-d6f6.html

-- Stirling number of the second kind
-- https://ja.wikipedia.org/wiki/%E3%82%B9%E3%82%BF%E3%83%BC%E3%83%AA%E3%83%B3%E3%82%B0%E6%95%B0
-- S 20 10 とかでも1.7秒ほどかかる O(2^n)? メモ化でO(nk)

-- stirling2 n k
--   | n == k    = 1
--   | k == 0    = 0
--   | otherwise = stirling2 (n-1) (k-1) + k * stirling2 (n-1) k

-- fast memoized
stirling2 n k = iter (n,k) where
  memo = listArray ((0,0),(n,k)) $ map iter $ range ((0,0),(n,k))
  iter (n,k)
    | n == k    = 1
    | k == 0    = 0
    | otherwise = memo!(n-1,k-1) + k * memo!(n-1,k)

-- fast memoized (safe)
stirling2' n k = iter (n,k) where
  memo = listArray ((0,0),(n,k)) $ map iter $ range ((0,0),(n,k))
  iter (n,k)
    | n < k     = undefined
    | n == k    = 1
    | k == 0    = 0
    | k == 1    = 1
    | otherwise = memo!(n-1,k-1) + k * memo!(n-1,k)

-- starling number of the first kind
-- fast memoized
stirling1 n k = iter (n,k) where
  memo = listArray ((0,0),(n,k)) $ map iter $ range ((0,0),(n,k))
  iter (n,k)
    | n == k    = 1
    | k == 0    = 0
    | otherwise = memo!(n-1,k-1) + (n-1) * memo!(n-1,k)

-- bell number
-- https://ja.wikipedia.org/wiki/%E3%83%99%E3%83%AB%E6%95%B0
-- fast memoized
bell n = iter (n,n) where
  memo = listArray ((1,1),(n,n)) $ map iter $ range ((1,1),(n,n))
  iter (1,_) = 1
  iter (2,1) = 1
  iter (n,1) = memo!(n-1,n-1)
  iter (n,k) = memo!(n-1,k-1) + memo!(n,k-1)
