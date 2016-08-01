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
-- import           Data.Array
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as C
-- import           Data.ByteString.Char8 ()
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap
-- import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
-- import qualified Data.Sequence as S

putList :: Show a => [a] -> IO ()
putList = putStrLn . unwords . map show

myread = read :: String -> Int
main = do
  [n,b,p] <- map myread.words <$> getLine
  putList $ solve n b p

solve :: Int -> Int -> Int -> [Int]
solve n b p = [bottles,towels] where
  bottles = matches n * (2 * b + 1) where
    matches 1 = 0
    matches 2 = 1
    matches !n = (k `div` 2) + matches (n - k + (k `div` 2)) where
      !k = last . takeWhile (<=n) . map (2^) $ [1..]
  towels = n * p