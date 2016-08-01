import Control.Applicative
import Control.Monad
import Data.List
import Data.Tuple

main = do
	-- n points, m segments
  [n,m] <- fmap (read :: String -> Int) . words <$> getLine
  ms <- fmap (fmap (read :: String -> Int) . words) <$> replicateM m getLine
  putStrLn $ solve ms

solve :: [[Int]] -> String
solve = maximum . map beauty subsequences where
	beauty :: [[Int]] -> Int
	beauty xs = (longest xs) * (snipes xs) where
		longest xs = 