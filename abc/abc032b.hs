import Control.Applicative
import Control.Monad
import Data.List
import Data.Tuple

main = do
  s <- getLine
  k <- readLn
  print $ solve s k

solve :: String -> Int -> Int
solve s k = length . nub $ substrings s k 

substrings s k = takeWhile ((==k).length) $ substr s k where
	substr [] _ = []
	substr s@(x:xs) k = take k s:(substrings xs k)