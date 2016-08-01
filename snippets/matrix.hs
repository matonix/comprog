{-
  Matrix.hs -- ベクトル及び行列の計算
                                     coding by Tsumuji
-}
module Matrix where
 
import Data.List (transpose)
 
 
--  ベクトルの演算
--  * ベクトルは数値のリストの形で表す。
--    ex : 3次元のベクトル [1,2,3]
 
type Vector a = [a]
 
-- Scalar * Vector
(*/) :: Num a => a -> Vector a -> Vector a
(*/) n xs = map (* n) xs
infixl 7 */
 
-- Vector + Vector
(/+/) :: Num a => Vector a -> Vector a -> Vector a
(/+/) xs ys = zipWith (+) xs ys
infixl 6 /+/
 
-- Vector - Vector
(/-/) :: Num a => Vector a -> Vector a -> Vector a
(/-/) xs ys = zipWith (-) xs ys
infixl 6 /-/
 
-- Vector * Vector (inner product)
(/*/) :: Num a => Vector a -> Vector a -> a
(/*/) xs ys = sum $ zipWith (*) xs ys
infixl 7 /*/
 
-- Vector の絶対値
vectorAbs :: Floating a => Vector a -> a
vectorAbs xs = sqrt (xs /*/ xs)
 
-- n 次元のゼロベクトル
zeroVector :: Num a => Int -> Vector a
zeroVector n = take n $ repeat 0
 
 
 
-- 行列の演算
-- * 行列は行ベクトルのリストの形で表す。
--   ex : 2行3列の行列 [[1,2,3],[4,5,6]]
-- * 行と列の整合性は調べていないので注意!!
 
type Matrix a = [Vector a]
 
-- 行列の m 行目 n 列目の要素（通常のリストと同様に 0 から数え始める）
matrixRef :: Num a => Int -> Int -> Matrix a -> a
matrixRef m n xss = (xss !! m) !! n
 
-- Scalar * Matrix
(*|) :: Num a => a -> Matrix a -> Matrix a
(*|) n xss = map (n */) xss
infixl 7 *|
 
-- Matrix + Matrix
(|+|) :: Num a => Matrix a -> Matrix a -> Matrix a
(|+|) xss yss = zipWith (/+/) xss yss
infixl 6 |+|
 
-- Matrix - Matrix
(|-|) :: Num a => Matrix a -> Matrix a -> Matrix a
(|-|) xss yss = zipWith (/-/) xss yss
infixl 6 |-|
 
-- Matrix * Matrix
(|*|) :: Num a => Matrix a -> Matrix a -> Matrix a
(|*|) xss yss = [[xs /*/ ys | ys <- yss'] | xs <- xss]
    where yss' = transpose yss
infixl 7 |*|
 
-- m 行 n 列のゼロ行列
zeroMatrix :: Num a => Int -> Int -> Matrix a
zeroMatrix m n = take m $ repeat $ zeroVector n
 
-- n 行 n 列の単位行列
unitMatrix :: Num a => Int -> Matrix a
unitMatrix n = [take n $ f x | x <- [0 .. n - 1]]
    where f x = replicate x 0 ++ [1] ++ repeat 0
 
-- Matrix ^ n （正方行列のみ可）
(|^) :: Num a => Matrix a -> Int -> Matrix a
(|^) xss 0 = unitMatrix (length xss)
(|^) xss n = power (|*|) xss n
infixr 8 |^
 
-- 高速累乗計算
-- ex : 2^100 == power (*) 2 100
power :: (t -> t -> t) -> t -> Int -> t
power op a n = loop (n - 1) a a
    where
      loop 0 _ r = r
      loop n x r = loop (div n 2) (op x x) r'
          where r' = if odd n then op r x else r