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

putList :: Show a => [a] -> IO ()
putList = putStrLn . unwords . map show

myread = read :: String -> Int
main = do
  _ <- getLine
  ks <- map myread.words <$> getLine
  putList $ solve ks

solve :: [Int] -> [Int]
solve [k] = [k,k]
solve ks = (head ks):(inter ks)
  where
    inter (x:[y]) = (min x y):[y]
    inter (x:y:ks) = (min x y):(inter (y:ks))