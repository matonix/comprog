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

type Point = (Int,Int)

-- myShow = show :: Int -> String 
myRead = read :: String -> Int
main = do
  [n,x,y] <- fmap myRead.words <$> getLine
  [xs,ys] <- transpose . fmap (fmap myRead.words) <$> replicateM n getLine
  print $ solve (x,y) (zip xs ys)

solve :: Point -> [Point] -> Double
solve n ns = size (maximum dists) (minimum dists)
  where
    size g l = pi*(g+l)*(g-l)
    dists = map (dist n) ns
      where
        dist :: Point -> Point -> Double
        dist (a,b) (c,d) = sqrt $ realToFrac ((c-a)^2+(d-b)^2)