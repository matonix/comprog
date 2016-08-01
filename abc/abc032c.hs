import Control.Applicative
import Control.Monad
import Data.List
import Data.Tuple

main = do
  [n,k] <- fmap (read :: String -> Int) . words <$> getLine
  s <- replicateM n readLn
  print $ solve k s

solve :: Int -> [Int] -> Int 
solve k = length . myhead . filter ((<=k).product) . substrings

substrings s = concat $ map (substrs s) [l,l-1..1] where
	l = length s

myhead [] = []
myhead x = head x

substrs :: [a] -> Int -> [[a]]
substrs s k = takeWhile ((==k).length) $ substr s k where
	substr [] _ = []
	substr s@(x:xs) k = take k s:(substr xs k)