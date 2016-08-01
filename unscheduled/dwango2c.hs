import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Tuple
import           Data.Int
import           Data.Char
import           Data.Function
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict  (IntMap)
-- import qualified Data.IntMap.Strict  as IntMap

myread = read :: String -> Int
main = do
  -- n:eki m:rosen
  [n,m,src,dst] <- map myread.words <$> getLine
  mss <- map (map myread.words) <$> replicateM (3*m) getLine
  print $ solve n m src dst mss

solve :: Int -> Int -> Int -> Int -> [[Int]] -> Int
solve n m src dst mss = 0