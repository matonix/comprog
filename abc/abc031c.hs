import Control.Applicative
import Control.Monad
import Data.List
import Data.Tuple

main = do
  _ <- getLine
  a <- fmap (read :: String -> Int) . words <$> getLine
  print $ solve a

solve :: [Int] -> Int
solve a = maximum $ map (score a) [1..n] where
	n = length a
	score a t = fst . max2 $ map (score2 a t) (delete t [1..n]) where
		score2 a t s = (\xs -> (sum $ odds xs, sum $ evens xs)) $ substr a t s

max2 :: Ord a => [(a,a)] -> (a,a)
max2 = foldl1 (\(x1,y1) (x2,y2) -> if y1 >= y2 then (x1,y1) else (x2,y2))

substr :: [a] -> Int -> Int -> [a]
substr xs a b
	| a > b = drop (b-1) $ take a xs
	| a <= b = drop (a-1) $ take b xs

odds :: [a] -> [a]
odds [] = []
odds (x:xs) = x : evens xs

evens :: [a] -> [a]
evens [] = []
evens (_:xs) = odds xs
