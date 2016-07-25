module Main where

main = do
  [n,x] <- map read . words <$> getLine
  print $ solve n x

solve :: Int -> Int -> Int
solve n x = 3 * go x (n-x) where
  go _ 0 = 0
  go x y = x`div`y * y + go y (x`mod`y)
