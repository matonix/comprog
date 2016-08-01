{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE TupleSections #-}

import           System.IO hiding (char8)
import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.List
import           Data.Tuple
import           Data.Int
import           Data.Char
import           Data.Function (on)
import           Data.Array
-- import           Data.UArray
-- import           Data.Array.IArray
import           Data.Array.ST
import           Data.Array.MArray
-- import           Data.Array.Unsafe
import           Data.Ix
import           Data.Maybe
import           Data.Monoid hiding ((<>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.ByteString.Builder
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntSet (IntSet)
-- import qualified Data.IntSet as S
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as M
import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
import qualified Data.Sequence as Seq
-- import           Debug.Trace

type Point = (Int,Int)

main = do
  (r,c) <- readInt2 <$> BS.getLine
  (sy,sx) <- readInt2 <$> BS.getLine
  (gy,gx) <- readInt2 <$> BS.getLine
  xs <- BL.concat . map BL.fromStrict <$> replicateM r BS.getLine
  print $ solve r c (sy,sx) (gy,gx) xs

solve :: Int -> Int -> Point -> Point -> BL.ByteString -> Int
solve r c s g xs = runST $ do
  a <- newListArray ((1,1),(r,c)) $ BL.unpack xs
  go s 0 Seq.empty a where
    go x c q a = if x == g
      then return c
      else go x' (c'+1) q' (d |> (c',x')) where
      ((c',x') :< q') = Seq.viewl $ q >< Seq.fromList (getpos x) where
        getpos x = zip (repeat c) $ filter ((=='.').(a!)) $ map (+++x) [(0,1),(1,0),(0,-1),(-1,0)]

-- solve :: Int -> Int -> Point -> Point -> BL.ByteString -> Int
-- solve r c s g xs = go s 0 Seq.empty Seq.empty where
--   a = listArray ((1,1),(r,c)) $ BL.unpack xs
--   go x c q d = if x == g
--     then c
--     else go x' (c'+1) q' (d |> (c',x')) where
--     ((c',x') :< q') = Seq.viewl $ q >< Seq.fromList (getpos x) where
--       getpos x = zip (repeat c) $ filter ((=='.').(a!)) $ map (+++x) [(0,1),(1,0),(0,-1),(-1,0)]


(a,b) +++ (c,d) = (a+c,b+d)

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  

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
           | SingleR Cell
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
renderRow (SingleR x) = renderCell x
renderRow (TupleR (x,y)) = renderCell x <> char8 ' ' <> renderCell y
renderRow (TripleR (x,y,z)) = renderCell x <> char8 ' ' <> renderCell y <> char8 ' ' <> renderCell z

renderCell :: Cell -> Builder
renderCell (ByteStringC cs) = lazyByteString cs
renderCell (IntC i) = intDec i
renderCell (Int64C i) = int64Dec i
