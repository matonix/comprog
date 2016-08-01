import Control.Applicative
import Control.Monad
import Data.List
import Data.Char

main = do
  [x,y] <- fmap (read :: String -> Int) . words <$> getLine
  ts <- fmap (fmap (read :: String -> Int) . words) <$> replicateM y getLine
  mapM_ (putStrLn . myShow) $ solve ts

solve :: [[Int]] -> [[Int]]
solve = transpose . map f . transpose
  where
    f xs = replicate a 0 ++ replicate b 1
      where
        a = length $ filter even xs
        b = length $ filter odd xs

myShow :: [Int] -> String
myShow [x] = [intToDigit x]
myShow (x:xs) = (intToDigit x):' ':(myShow xs)