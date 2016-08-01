import Control.Applicative
import Control.Monad
import Data.List
import Data.Tuple

main = do
  [m,n] <- fmap (read :: String -> Int) . words <$> getLine
  ns <- fmap (fmap (read :: String -> Int) . words) <$> replicateM m getLine
  putStrLn $ solve n ns

solve :: Int -> [[Int]] -> String
solve n ns = if (length $ nub $ foldl1 union ns) == n then "YES" else "NO"