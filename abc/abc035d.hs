{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE TupleSections #-}

import           System.IO hiding (char8)
import           Control.Applicative
import           Control.Monad
import           Control.Arrow
import           Data.List
import           Data.Tuple
import           Data.Int
import           Data.Char
import           Data.Function
import           Data.Array.Unboxed
import           Data.Array.IArray
import           Data.Array.ST
import           Data.Array.MArray
import           Data.Ix
import           Data.Maybe
import           Data.Tree
import           Data.Monoid hiding ((<>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.ByteString.Builder
import           Data.Array.Unsafe
-- import           Data.Vector.Unboxed.Mutable ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed.Mutable as MV
-- import qualified Data.Vector as V
-- import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
-- import qualified Data.Sequence as Seq
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- import           Data.Set (Set)
-- import qualified Data.Set as Set

out = stdout

main = do
  (n,m,t) <- readInt3 <$> BS.getLine
  as <- readIntN <$> BS.getLine
  abcs <- map readInt3 <$> replicateM m BS.getLine
  print $ solve n m t as abcs

solve :: Int -> Int -> Int -> [Int] -> [(Int,Int,Int)] -> Int
solve n m t as abcs = maxValue * remainTime where
  (maxValue, t) = maximumBy (compare `on` fst) $ zip as [1..]
  remainTime = t - (dijkstra g n 1 t) - (dijkstra g n t 1) where
    g = graph abcs n

type Graph = Map Vertex (Map Vertex Cost)
type Vertex = Int
type Cost = Int

graph :: [(Vertex,Vertex,Cost)] -> Int -> Graph
graph xs n = Map.fromListWith $ uncurry Map.insert $ map (\(a,b,c) -> (a,(b,c))) xs

adjacent :: Graph -> Vertex -> Map Vertex Cost
adjacent g v = fromJust $ Map.lookup v g

data Priority Cost Vertex deriving (Eq,Show)
instance Ord Priority where
  Priority c1 _ = `compare` Priority c2 _ = c1 `compare` c2

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
