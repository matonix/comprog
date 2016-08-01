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
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap
-- import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
-- import qualified Data.Sequence as S

myread = read :: String -> Integer
main = do
  n <- readLn
  ns <- map myread.words <$> getLine
  print $ solve n ns 

solve :: Int -> [Integer] -> Integer
solve _ ns = go (group . sort $ ns) where
  go ns = if all ((==1) . length) ns
    then sum . concat $ ns
    else go (group . sort . concat . map stair $ ns) where
      stair [] = []
      stair xs = take (length xs) [maximum xs,maximum xs - 1..0]