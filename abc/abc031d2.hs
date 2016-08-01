import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Array
import           Data.Tuple
import           Data.Int
import           Data.Char
import           Data.Function
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict  (IntMap)
-- import qualified Data.IntMap.Strict  as IntMap

myread = read :: String -> Int
main = do
  [k,n] <- map myread.words <$> getLine
  [v',w'] <- transpose . map words <$> replicateM n getLine
  v <- return $ map (map digitToInt) v'
  w <- return w'
  mapM_ putStrLn $ solve k v w

solve :: Int -> [[Int]] -> [String] -> [String]
solve k v w = (answer (concat v) (concat w) . (\(Just x) -> x)) $ find (matcher v w) (brute [1..3] k)

brute :: [a] -> Int -> [[a]]
brute x n = iterate (liftA2 (:) x) [[]] !! n

matcher :: [[Int]] -> [String] -> [Int] -> Bool
matcher vs ws ns = all (p ns) (zip vs ws)
  where
    p :: [Int] -> ([Int],String) -> Bool
    p ns (vs,ws) = sum (map ((0:ns)!!) vs) == length ws

answer :: [Int] -> String -> [Int] -> [String]
answer vs ws ns = map snd . sortBy (compare `on` fst) . nub $ f vs (map ((0:ns)!!) vs) ws
  where
    f :: [Int] -> [Int] -> String -> [(Int,String)]
    f [] [] [] = []
    f (v:vs) (l:ls) ws = (v,take l ws):(f vs ls (drop l ws))
