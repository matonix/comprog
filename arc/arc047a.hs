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
  [n,l] <- fmap myRead.words <$> getLine
  ss <- getLine
  print $ solve l ss

solve :: Int -> String -> Int
solve l ss = f 0 1 l ss
  where
    f c _ _ [] = c
    f c t l (s:ss)
      | s=='+' && t==l = f (c+1) 1 l ss
      | s=='+' && t<l = f c (t+1) l ss
      | s=='-' = f c (t-1) l ss