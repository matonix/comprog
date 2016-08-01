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
-- import           Data.Array.ST
-- import           Data.Array.Unsafe
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap
-- import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
-- import qualified Data.Sequence as S

out = stdout
fin = stdin

main = do
  (n,k) <- readInt2 <$> BS.hGetLine fin
  wps <- map (toTuple . map rd . BS.words) <$> replicateM n (BS.hGetLine fin)
  print $ solve n k wps

solve :: Int -> Int -> [(Double,Double)] -> Double
-- solve n k wps = maximum . map (snd . foldl (%%) (0,0)) $ comb5 wps k
solve n k wps = bin 100 50 where
  bin p w = if w < 0.0000005 then p else if f p
    then bin (p+w) (w/2)
    else bin (p-w) (w/2)
  f p = (>=0) . sum . take k . reverse . sort $ map (prioritize p) wps where
    prioritize q (w,p) = w*(p-q)

-- comb5 :: [a] -> Int -> [[a]]
-- comb5 [] _ = [[]]
-- comb5 ns size = comb' size [(ns, [])]
--     where
--       comb' 0 xs = [a | (_, a) <- xs]
--       comb' c xs = comb' (c - 1) $ concatMap comb'' xs
--       comb'' (x : xs, ys) = (xs, ys ++ [x]) : comb'' (xs, ys)
--       comb'' _ = []

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  

rd :: BS.ByteString -> Double
rd = fromIntegral . fst . fromJust . BS.readInt 

readInt1 :: BS.ByteString -> Int
readInt1 = fst . fromJust . BS.readInt 

readInt2 :: BS.ByteString -> (Int,Int)
readInt2 = toTuple . readIntN

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
