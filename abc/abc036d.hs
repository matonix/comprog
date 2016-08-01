{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE TupleSections #-}

import           System.IO hiding (char8)
import           Control.Applicative
import           Control.Monad
import           Data.Graph (Graph,Edge)
import qualified Data.Graph as G
import           Data.Tree
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
import           Data.Array.ST
-- import           Data.Array.IArray
import           Data.Array.MArray
-- import           Data.Array.Unsafe
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntSet (IntSet)
-- import qualified Data.IntSet as S
import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as M
-- import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
-- import qualified Data.Sequence as S
-- import           Debug.Trace

data Color = WB | W deriving (Eq, Show, Ord, Ix)

out = stdout

main = do
  n <- readInt1 <$> BS.getLine
  ns <- map readInt2 <$> replicateM (n-1) BS.getLine
  print $ solve n ns

solve :: Int -> [(Int,Int)] -> Int64
solve n ns = go (WB,1) where
  adj = flip dfs 1 $ adjacencyListUndirected (1,n) ns
  memo = listArray ((WB,1),(W,n)) $ go <$> range ((WB,1),(W,n))
  go (WB,v) = if null (adj!v) then 2::Int64
    else go (W,v) + productmod (map (curry go W) (adj!v))
  go (W,v)  = if null (adj!v) then 1::Int64
    else productmod (map (curry go WB) (adj!v))

adjacencyListUndirected :: (Int,Int) -> [(Int,Int)] -> Array Int [Int]
adjacencyListUndirected bounds es = runSTArray $ do
  a <- newArray bounds []
  mapM_ (\(i,j) ->
    readArray a i >>= writeArray a i . (j:) >>
    readArray a j >>= writeArray a j . (i:)) es
  return a

dfs :: Array Int [Int] -> Int -> Array Int [Int]
dfs a' v = runSTArray $ do
  a <- thaw a'
  go [] a v where
    go d a v = do
      adj <- readArray a v
      writeArray a v (adj\\d)
      mapM_ (go (v:d) a) (adj\\d)
      return a
    
-- solve :: Int -> [(Int,Int)] -> Int64
-- solve n ns = wb t where
--   t = head . flip G.dfs [1] $ G.buildG (1,n) (ns ++ map swap ns)
--   wb (Node v vs) = w (Node v vs) + productmod (map w vs)
--   wb (Node v []) = 2::Int64
--   w  (Node v vs) = productmod (map wb vs)
--   w  (Node v []) = 1::Int64

-- makeTable :: -> Table
-- makeTable 

-- mod modules --

modNum = 1000000007

modd a = a `mod` modNum

(+%) a b = modd (modd a + modd b)
infixl 6 +%

(-%) a b = modd (modd a - modd b + modNum)
infixl 6 -%

(*%) a b = modd (modd a * modd b)
infixl 7 *%

productmod = foldl (*%) 1

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
