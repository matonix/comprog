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
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n = length $ takeWhile (<=n) [x*25 | x<-[1..]]