{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE TupleSections #-}

import           System.IO hiding (char8)
import           Control.Applicative
import           Control.Monad
import           Debug.Trace
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

type Point = (Int,Int)
-- data Triangle = Acute | Righte | Obtuse deriving Show

out = stdout

main = do
  n <- readInt1 <$> BS.getLine
  ps <- map readInt2 <$> replicateM n BS.getLine
  putAns out . makeTable $ solve n ps

solve :: Int -> [Point] -> (Int,Int,Int)
solve n ps = (a,r,o) where
  a = t - r - o
  o = t * 3 - aa - r
  t = bino n 3
  (aa,r) = foldl1 (binTuple (+) (+)) $ map go ps where
    go p = go' 0 1 1 0 0 where
      -- trace ("go p:"++show p)
      go' i j k a r
        | i == n-1  = (a,r)
        | isA i j   = go' i (j+1) (k+(j>=k?(1,0))) a r
        | isR i k   = go' i j (k+1) a r
        | otherwise = go' (i+1) j k (a+j-i-1) (r+k-j)
        -- where trgo' = trace ("go' (i,j,k,a,r):"++show (i,j,k,a,r))
      isA s t = as!s + pi/2 ~> as!t
      isR s t = as!s + pi/2 ~== as!t
      as = listArray (0,2*(n-1)-1) (ps' ++ map (+pi*2) ps') where
        ps' = sort . map (angle p) . filter (/=p) $ ps where 


-- solve :: Int -> [Point] -> (Int,Int,Int)
-- solve n ps = go ps (0,0) where
--   go [] (r,o) = (bino n 3-r-o,r,o)
--   go (p:qs) (r,o) = go qs (r+r',o+o') where
--     r' = go' isA isR
--     o' = go' isAR isO
--     go' p q = go'' 0 0 0 0 where
--       go'' j a b v
--         | j == n-1  = v
--         | p j a     = go'' j (a+1) (b+if a>=b then 1 else 0) v
--         | q j b     = go'' j a (b+1) v
--         | otherwise = go'' (j+1) a b (v+b-a)
--         -- where trgo' = trace ("go'' j:"++show j++" a:"++show a++" b:"++show b++" v:"++show v)
--     isA s t = as!s + pi/2 ~> as!t
--     isR s t = as!s + pi/2 ~== as!t
--     isAR s t = as!s + pi/2 ~>= as!t
--     isO s t = as!s + pi ~> as!t
--     as = listArray (0,2*(n-1)-1) (ps' ++ map (+pi*2) ps') where
--       ps' = sort . map (angle p) . filter (/=p) $ ps where

(?) :: Bool -> (a,a) -> a
x ? (a, b) = if x then a else b
infix 3 ?

angle :: Point -> Point -> Double
angle (sx,sy) (dx,dy) = atan2 (fromIntegral (dy-sy)) (fromIntegral (dx-sx))

eps = 1e-10

(~==) :: Double -> Double -> Bool
a ~== b = abs (a - b) < eps
infix 4 ~==

(~<) :: Double -> Double -> Bool
a ~< b = b - a > eps
infix 4 ~<

(~<=) :: Double -> Double -> Bool
a ~<= b = b - a > -eps
infix 4 ~<=

(~>) :: Double -> Double -> Bool
a ~> b = a - b > eps
infix 4 ~>

(~>=) :: Double -> Double -> Bool
a ~>= b = a - b > -eps
infix 4 ~>=

bino n k = bino' n (if k*2 <= n then k else n-k) where
  bino' _ 0 = 1
  bino' 0 _ = 0
  bino' n k = bino (n-1) (k-1) * n `div` k 

-- solve :: [Point] -> (Int,Int,Int)
-- solve = count . map tri . comb 3

comb :: Int -> [a] -> [[a]]
comb _ [] = [[]]
comb size ns = comb' size [(ns, [])]
    where
      comb' 0 xs = [a | (_, a) <- xs]
      comb' c xs = comb' (c - 1) $ concatMap comb'' xs
      comb'' (x : xs, ys) = (xs, ys ++ [x]) : comb'' (xs, ys)
      comb'' _ = []

-- tri :: [Point] -> Triangle
-- tri = judge . sort . map len2 . comb 2 where
--   len2 [(a,b),(c,d)] = (a-c)^2+(b-d)^2
--   judge [a,b,c]
--     | a+b>c = Acute
--     | a+b<c = Obtuse
--     | otherwise = Righte

-- count :: [Triangle] -> (Int,Int,Int)
-- count = count' 0 0 0 where
--   count' a r o []          = (a,r,o)
--   count' a r o (Acute :ts) = count' (a+1) r o ts
--   count' a r o (Righte:ts) = count' a (r+1) o ts
--   count' a r o (Obtuse:ts) = count' a r (o+1) ts

makeTable :: (Int,Int,Int) -> Table
makeTable (a,b,c) = [TripleR (IntC a, IntC b, IntC c)]

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

binTuple :: (a -> a' -> a'') -> (b -> b' -> b'') -> (a, b) -> (a', b') -> (a'', b'')
binTuple op1 op2 (x, y) (z, w) = (op1 x z, op2 y w)

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
