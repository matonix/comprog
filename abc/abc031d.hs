import Control.Applicative
import Control.Monad
import Data.List
import Data.Char
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

main = do
  [k,n] <- map read . words <$> getLine
  [v',w] <- transpose . map words <$> replicateM n getLine
  v <- map (read :: String -> Int) <$> return v'
  mapM_ putStrLn $ solve (zip v w)

-- k:整数のアルファベット数 n:情報の数 v:整数列 w:対応する文字列
solve :: [(Int,String)] -> [String]
solve = IntMap.elems . head . foldl1 unionmaps . map maps

unionmaps :: [IntMap String] -> [IntMap String] -> [IntMap String]
unionmaps xms yms = filter consistent [IntMap.unionWith f xm ym | xm <- xms, ym <- yms]
  where
    consistent = all (/=[]) . IntMap.elems
    f lv rv = if lv == rv then lv else []

maps :: (Int,String) -> [IntMap String]
maps (v,w) = map (IntMap.fromList . zip sv) (splits (length sv) w)
  where
    sv = map digitToInt $ show v

splits :: Int -> String -> [[String]]
splits n xs = filter consistent $ map (words . insertB ' ' xs) (genBools (length xs - 1) (n-1))
  where
    consistent = all ((<=3) . length)

-- a list of length n which has m "True"s
genBools :: Int -> Int -> [[Bool]]
genBools n m = nub $ permutations [x <= m | x <- [1..n]]

insertB :: a -> [a] -> [Bool] -> [a]
insertB x [y] [] = [y]
insertB x (y:ys) (b:bs) = if b then y:x:(insertB x ys bs) else y:(insertB x ys bs)
