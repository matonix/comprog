import Control.Applicative
import Control.Monad
import Data.List
import Data.Tuple
import Data.Sequence ((|>), ViewL ((:<)))
import qualified Data.Sequence as Seq


main = do
  [n,k] <- fmap (read :: String -> Int) . words <$> getLine
  ss <- replicateM n readLn
  print $ solve k ss

solve :: Int -> [Int] -> Int
solve k ss = if elem 0 ss then length ss
	else syakutori k ss

syakutori :: Int -> [Int] -> Int
syakutori k ss = syaku k ss 1 Seq.empty 0
  where
    syaku :: Int -> [Int] -> Int -> Seq.Seq Int -> Int -> Int
    syaku _ [] _ _ m = m
    syaku k (s:ss) n qs m
      | n*s<=k = syaku k ss (n*s) (qs|>s) (max m (Seq.length qs+1))
      | s>=k = syaku k ss 1 Seq.empty m
      | otherwise = syaku k (s:ss) (n`div`q) (Seq.drop 1 qs) m
        where
          q = (\(x:<xs) -> x) $ Seq.viewl qs