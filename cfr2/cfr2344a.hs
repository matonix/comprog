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
import           Data.Bits
import           Data.Array
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as C
-- import           Data.ByteString.Char8 ()
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap
import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
import qualified Data.Sequence as S

myread = read :: String -> Int
main = do
  _ <- getLine
  as <- map myread.words <$> getLine
  bs <- map myread.words <$> getLine
  print $ solve as bs

solve :: [Int] -> [Int] -> Int
solve as bs = foldl1 (.|.) as + foldl1 (.|.) bs