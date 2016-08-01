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
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as C
-- import           Data.ByteString.Char8 ()
import           Data.Vector.Unboxed ((//), (++), (!), (!?))
import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap
-- import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
-- import qualified Data.Sequence as S


myread = read :: String -> Int
main = do
  [n,m,k] <- map myread.words <$> getLine
  as <- map myread.words <$> getLine
  bs <- map myread.words <$> getLine
  print $ solve k (U.fromList as,U.fromList bs)

solve :: Int -> (U.Vector Int,U.Vector Int) -> Int
solve k (as,bs) = maximum $ map score [swap n m (as,bs) | n <- [0..U.length as], m <- [0..U.length bs]]
  where
    score (as,bs) = (U.sum as) * (U.sum bs)
    swap n m (as,bs) = (as // [(n,bs ! m)],bs // [(m,as ! n)])
