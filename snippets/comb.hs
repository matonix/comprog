{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Array
import           Data.Array.IO

myread = read :: String -> Integer
main = do
  [m,n] <- map myread.words <$> getLine
  print $ solve m n

solve :: Integer -> Integer -> Integer
solve = comb

comb :: Integer -> Integer -> Integer
comb n r
  | r == n || r == 0 = 1
  | otherwise = comb (n-1) r + comb (n-1) (r-1)

comb2 :: Integer -> Integer -> Integer
comb2 n r = iter (n,r) where
  memo = listArray ((0,0),(n,r)) $ fmap iter [(nn,rr) | nn <- [0..n], rr <- [0..r]]
  iter (nn,rr)
    | rr == nn || rr == 0 = 1
    | otherwise = memo ! (nn-1,rr) + memo ! (nn-1,rr-1)

comb3 :: Int -> Int -> IO Integer
comb3 n r = do
  a <- newArray ((0,0),(n,n)) 1 :: IO (IOArray (Int,Int) Integer)
  forM_ [2..n] $ \i ->
    forM_ [1..i-1] $ \j -> do
      b <- readArray a (i-1,j)
      c <- readArray a (i-1,j-1)
      writeArray a (i,j) (b+c)
  readArray a (n,r)

comb4 n r = iterate (scanl1 (+)) [1,1..] !! (n-r) !! r