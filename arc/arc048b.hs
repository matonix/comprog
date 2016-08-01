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

type Rating = Int
data Hand = Go | Ch | Pa deriving (Eq, Show)

readhand :: Int -> Hand
readhand 1 = Go
readhand 2 = Ch
readhand 3 = Pa

putList :: Show a => [a] -> IO ()
putList = putStrLn . unwords . map show

myread = read :: String -> Int
main = do
  n <- readLn
  [rs,hs] <- transpose . map (map myread.words) <$> replicateM n getLine
  mapM_ putList $ solve n (zip3 [1..] rs (map readhand hs))

solve :: Int -> [(Int,Rating,Hand)] -> [[Int]]
solve n xs = go (groupOn snd3 . sortOn snd3 $ xs) (listArray (1,n) $ repeat [0]) 0 where
  go [] as win = elems as
  go (y:ys) as wins = go' (groupOn trd3 y) as wins


   (as//[(i,[wins,loses,draws]) | i <-fst3.unzip3$y]) (wins+length y) where
    draws = length y
    loses = n - wins - loses

fst3 (x,_,_) = x
snd3 (_,y,_) = y
trd3 (_,_,z) = z
sortOn at = sortBy (compare `on` at)
groupOn at = groupBy ( (==) `on` at)