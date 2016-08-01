import Control.Applicative
import Control.Monad
import Data.List
import Data.Tuple

main = do
  a <- readLn
  b <- readLn
  n <- readLn
  print $ solve a b n

solve :: Int -> Int -> Int -> Int
solve a b n = head $ filter (\x -> (x>=n) && (modd a x) && (modd b x)) [1..]

modd n = \x -> x`mod`n==0