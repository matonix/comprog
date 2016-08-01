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
myRead = read :: String -> Int
main = do
  n <- readLn
  ns <- fmap myRead.words <$> getLine
  ans <- return (solve n ns)
  print $ length ans
  putStrLn $ head ans

solve :: Int -> [Int] -> [String]
solve n ns = zip ns ['a'..'z']