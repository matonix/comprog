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

solve n = if n `mod` 5 == 0 then n `div` 5 else n `div` 5 + 1