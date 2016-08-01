import Control.Applicative
import Control.Monad
import Data.List
import Data.Tuple

main = do
  [n,k] <- fmap (read :: String -> Int) . words <$> getLine
  s <- replicateM n readLn
  print $ solve k s

solve :: Int -> [Int] -> Int
solve k s = if elem 0 s then length s
	else maximum $ map (length . myTakeWhile k) (tails s)

myTakeWhile :: (Ord t, Num t) => t -> [t] -> [t]
myTakeWhile k xs = f 1 k xs where
	f n k [] = []
	f n k (x:xs) = if n*x<=k
	then x:(f (n*x) k xs)
	else []