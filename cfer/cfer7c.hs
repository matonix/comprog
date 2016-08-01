{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Tuple
import           Data.Int
import           Data.Char
import           Data.Function
import           Data.Array
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as C
-- import           Data.ByteString.Char8 ()
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap
-- import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
-- import qualified Data.Sequence as S

myread = read :: String -> Int
main = do
  [n,m] <- map myread.words <$> getLine
  as <- map myread.words <$> getLine
  qs <- map ((\[l,r,x] -> (l,r,x)).map myread.words) <$> replicateM m getLine
  mapM_ print $ solve as qs

solve :: [Int] -> [(Int,Int,Int)] -> [Int]
solve as qs = map f qs where
  f (l,r,x) = g . find (p l r x) $ (zip [1..] as) where
    p l r x (i,a) = l<=i && i<=r && x/=a
    g (Just (x,_)) = x
    g Nothing = -1