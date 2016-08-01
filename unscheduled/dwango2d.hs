import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Tuple
import           Data.Int
import           Data.Char
import           Data.Function
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict  (IntMap)
-- import qualified Data.IntMap.Strict  as IntMap

type Matrix = V.Vector (U.Vector Int)

myread = read :: String -> Int
main = do
  [h,w] <- map myread.words <$> getLine
  bss <- map (map myread.words) <$> replicateM h getLine
  print $ solve h w (fromList bss)

solve :: Int -> Int -> Matrix -> Int
solve h w bss = mapMatrix 

mapMatrix :: (Int -> Int) -> Matrix -> Matrix
mapMatrix f = V.map (U.map f)

foldlMatrix :: (Int -> Int -> Int) -> Matrix -> Int
foldlMatrix op = V.foldl1 op . (U.foldl1 op)

fromList :: [[Int]] -> Matrix
fromList = V.fromList . map U.fromList

brute x n = iterate (liftA2 (:) x) [[]] !! n