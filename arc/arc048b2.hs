{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Tuple
import           Data.Int
import           Data.Char
import           Data.Function
import           Data.Array
import           Data.Array.ST
import           Data.Array.Unsafe
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.ByteString.Char8 ()
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap
-- import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
-- import qualified Data.Sequence as S

type Rate = Int
type Hand = Int
weakerThan 1 = 2
weakerThan 2 = 3
weakerThan 3 = 1

putList :: Show a => [a] -> IO ()
putList = putStrLn . unwords . map show

readInt = read :: String -> Int
toTuple = \[x,y] -> (x,y)
main = do
  n <- readLn
  rhs <- map (toTuple . map readInt . words) <$> replicateM n getLine
  mapM_ (putList . \(x,y,z) -> [x,y,z]) $ solve n rhs

solve :: Int -> [(Rate,Hand)] -> [(Int,Int,Int)]
solve n rhs = map (\rh -> as!rh) rhs where
  as :: Array (Rate,Hand) (Int,Int,Int)
  as = listArray ((0,1),(100000,3)) $ count (assocs bucket) 0 where
    bucket :: Array (Rate,Hand) Int
    bucket = accumArray (+) 0 ((0,1),(100000,3)) (zip rhs $ repeat 1)
    count :: [((Rate,Hand),Int)] -> Int -> [(Int,Int,Int)]
    count [] _ = []
    count (((r,h),m):bs) won = (win,lose,draw):(count bs newin) where
      win  = won + bucket!(r,weakerThan h) 
      draw = m - 1
      lose = n - win - draw - 1
      newin = won + if h==3 then bucket!(r,1)+bucket!(r,2)+bucket!(r,3) else 0

update :: (Ix a) => Array a b -> a -> b -> Array a b
update ary x y = runSTArray $ do
                   ary' <- unsafeThaw ary
                   writeArray ary' x y
                   return ary'
