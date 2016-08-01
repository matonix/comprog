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

myRead = read :: String -> Int64
main = do
  [l,r,k] <- fmap myRead.words <$> getLine
  putStrLn . myShow $ solve l r k

solve l r k = takeWhile (<=r) $ dropWhile (<l) $ map (k^) [0..]

mshow = show :: Int64 -> String 
myShow :: [Int64] -> String
myShow [] = "-1"
myShow [x] = mshow x
myShow (x:xs) = (mshow x) ++ " " ++ (myShow xs)
