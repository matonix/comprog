import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Array
import           Data.Tuple
import           Data.Int
import           Data.Char
-- import           Data.Function
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict  (IntMap)
-- import qualified Data.IntMap.Strict  as IntMap

-- myShow = show :: Int -> String 
myRead = read :: String -> Int64
main = do
  [n,k] <- fmap myRead.words <$> getLine
  mapM_ print $ solve n k

solve :: Int64 -> Int64 -> [Int64]
-- solve n k = (sort $ permutations [1..n]) !! (product ([1..n] \\ [k]) - 1)
solve n k = f (product (delete k [1..n]) - 1) (n-1) [1..n]
  where
    f _ 0 [r] = [r]
    f p q r = s:f (mod p (power q)) (q-1) (delete s r)
      where
        s = r !! (div p (power q))

power n = product [1..n]

