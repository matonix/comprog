{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Tuple
import           Data.Int
import           Data.Char
import           Data.Function
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict  (IntMap)
-- import qualified Data.IntMap.Strict  as IntMap

type Point = (Int,Int)

myread = read :: String -> Int
main = do
  [n,x1,y1,x2,y2] <- map myread.words <$> getLine
  [xs,ys] <- transpose . map (map myread.words) <$> replicateM n getLine
  print $ solve (x1,y1) (x2,y2) (zip xs ys)

sqdist :: Point -> Point -> Integer
sqdist (a,b) (c,d) = (toInteger (a-c))^2+(toInteger (b-d))^2

-- solve :: Point -> Point -> [Point] -> Integer
-- solve t1 t2 fs = go t1 t2 fs 0 0
--   where
--     go _  _  [] r1 r2 = r1+r2
--     go t1 t2 (f:fs) r1 r2 = go t1 t2 fs newr1 newr2
--       where
--         c1 = max (sqdist t1 f) r1
--         c2 = max (sqdist t2 f) r2
--         newr1 = if c1+r2 < r1+c2 then c1 else r1
--         newr2 = if r1+c2 < c1+r2 then c2 else r2            

brute x n = iterate (liftA2 (:) x) [[]] !! n

solve :: Point -> Point -> [Point] -> Integer
solve t1 t2 = maximum . bru . clean . tupleDist
  where 
    tupleDist = map (\x -> (sqdist t1 x,sqdist t2 x))
    clean = clean' . sortBy (compare `on` fst)
      where
        clean' (x:[y])
          | snd x <= snd y = [y]
          | otherwise = (x:[y])
        clean' (x:y:ys)
          | snd x <= snd y = clean' (y:ys)
          | otherwise = x:(clean' (y:ys))
    -- dame ^o^
    bru :: [(Integer,Integer)] -> [Integer]
    bru xs = map (sum . zipWith (flip ($)) xs) $ brute [fst,snd] (length xs)