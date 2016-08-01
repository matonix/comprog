{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE TupleSections #-}

import           System.IO hiding (char8)
import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Tuple
import           Data.Int
import           Data.Char
import           Data.Function (on)
import           Data.Array
import           Data.Ix
import           Data.Maybe
import           Data.Monoid hiding ((<>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.ByteString.Builder
import           Control.Monad.ST
import           Data.Array.ST
-- import           Data.Array.Unsafe
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as MS
-- import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
-- import qualified Data.Sequence as S

out = stdout

main = do
  (n,w) <- readInt2 <$> BS.getLine
  vws <- map readInt2 <$> replicateM n BS.getLine
  print $ solve n w (unzip vws)

solve :: Int -> Int -> ([Int], [Int]) -> Int64
solve n w (vs,ws)
  | n <= 30             = halfBrute w (zip vs ws)
  | maximum ws <= 1000  = weightMemoize w n (listArray (0,n-1) vs) (listArray (0,n-1) ws)
  | maximum vs <= 1000  = valueDP w n (listArray (0,n-1) vs) (listArray (0,n-1) ws)
  | otherwise = undefined

tuplePlus (x1,y1) (x2,y2) = (x1+x2,y1+y2)

halfBrute :: Int -> [(Int,Int)] -> Int64
halfBrute w = merge w . map (brute w) . halve
  where
    halve xs = [take n xs,drop n xs] where n = length xs `div` 2

    brute :: Int -> [(Int,Int)] -> [(Int64,Int64)]
    brute w = clean . sortOnWeight . map tupleSum . subsequences . map fromIntegrals
      where
        fromIntegrals = applyTuple fromIntegral fromIntegral
        tupleSum = foldl tuplePlus (0,0)
        sortOnWeight = sortBy (compare `on` snd)
        clean xs = clean' xs (-1)
          where
            clean' [] _ = []
            clean' (x:xs) maxv
              | fst x > maxv = x:clean' xs (fst x)
              | otherwise = clean' xs maxv

    merge :: Int -> [[(Int64,Int64)]] -> Int64
    merge w [pre,post] = maximum $ map (pair w (weightfilter pre)) (weightfilter post)
      where
        weightfilter = filter ((<=fromIntegral w) . snd)
        pair w pre post = last . fst . unzip . weightfilter $ map (tuplePlus post) pre

weightMemoize :: Int -> Int -> Array Int Int -> Array Int Int -> Int64
weightMemoize w n vs ws = iter (n-1,min w c) where
  c = n*1000
  memo :: Array (Int,Int) Int64
  memo = listArray ((0,0),(n-1,c)) $ iter <$> range ((0,0),(n-1,c))
  iter (0,0) = 0
  iter (0,_) = minBound
  iter (n',c') = max unstack stack where
    unstack = memo!(n'-1,c')
    stack = if c'-ws!n' >= 0
      then memo!(n'-1,c'-ws!n') + fromIntegral (vs!n')
      else 0

valueDP :: Int -> Int -> Array Int Int -> Array Int Int -> Int64
valueDP w n vs ws = fromIntegral . fst . last . filter (\(_,a) -> a<=w) $ arr where 
  arr = runST $ do
    a <- newListArray (0,n*1000) (0:repeat maxBound) :: ST s (STUArray s Int Int)
    forM_ [1..n] $ \i ->
      forM_ (reverse [vs!i..n*1000]) $ \j -> do
        aj <- fromIntegral <$> readArray a j
        aji <- fromIntegral <$> readArray a (j-(vs!i))
        writeArray a j (min aj (aji + fromIntegral (ws!i)))
    getAssocs a

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  

readInt1 :: BS.ByteString -> Int
readInt1 = fst . fromJust . BS.readInt 

readInt2 :: BS.ByteString -> (Int,Int)
readInt2 = toTuple . readIntN

readInt642 :: BS.ByteString -> (Int64,Int64)
readInt642 = toTuple . map (fromIntegral . readInt1) . BS.words

readInt3 :: BS.ByteString -> (Int,Int,Int)
readInt3 = toTriple . readIntN

readIntN :: BS.ByteString -> [Int]
readIntN =  map readInt1 . BS.words

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)

toTriple :: [a] -> (a, a, a)
toTriple [x, y, z] =(x, y, z)

fromTuple :: (a, a) -> [a]
fromTuple (x, y) = [x, y]

fromTriple :: (a, a, a) -> [a]
fromTriple (x, y, z) = [x, y, z]

-- if not applying, use "const"

applyTuple :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
applyTuple f g (x, y) = (f x, g y)

applyTriple :: (a -> a') -> (b -> b') -> (c -> c') -> (a, b, c) -> (a', b', c')
applyTriple f g h (x, y, z) = (f x, g y, h z)

-- output functions

data Cell  = ByteStringC BL.ByteString
           | IntC Int
           | Int64C Int64
           deriving( Eq, Ord, Show )

data Row   = ListR [Cell]
           | TupleR (Cell,Cell)
           | TripleR (Cell,Cell,Cell)
           deriving( Eq, Ord, Show )

type Table = [Row]

infixr 4 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

putAns :: Handle -> Table -> IO ()
putAns o = hPutBuilder o . renderTable

renderTable :: Table -> Builder
renderTable rs = mconcat [renderRow r <> char8 '\n' | r <- rs]

renderRow :: Row -> Builder
renderRow (ListR []) = mempty
renderRow (ListR (c:cs)) = renderCell c <> mconcat [ char8 ' ' <> renderCell c' | c' <- cs ]
renderRow (TupleR (x,y)) = renderCell x <> char8 ' ' <> renderCell y
renderRow (TripleR (x,y,z)) = renderCell x <> char8 ' ' <> renderCell y <> char8 ' ' <> renderCell z

renderCell :: Cell -> Builder
renderCell (ByteStringC cs) = lazyByteString cs
renderCell (IntC i) = intDec i
renderCell (Int64C i) = int64Dec i
