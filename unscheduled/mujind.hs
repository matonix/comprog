{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Tuple
import           Data.Int
import           Data.Char
import           Data.Function
import           Data.Array
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as C
-- import           Data.ByteString.Char8 ()
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap
-- import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
-- import qualified Data.Sequence as S

myread = read :: String -> Int
main = do
  n <- readLn
  s <- getLine
  q <- readLn
  qs <- map ((\[x,y] -> (x,y)).map myread.words) <$> replicateM q getLine
  mapM_ (putStrLn.yn) $ solve n s qs

yn b = if b then "Yes" else "No"

solve :: Int -> String -> [(Int,Int)] -> [Bool]
solve n s = map (isWF . substring s)

substring s (x,y) = drop (x-1) . take y $ s

-- isWF s = go s 0 where
--   go [] 0 = True
--   go [] _ = False
--   go ( '(' :xs) n = go xs (n+1)
--   go ( ')' :xs) 0 = False
--   go ( ')' :xs) n = go xs (n-1)
--   go ( '?' :xs) 0 = go xs 1
--   go ( '?' :xs) n = go xs (n+1) || go xs (n-1)

-- isWF s = go (0,0) where
--   !mx = length s-1
--   !sa = listArray (0,mx) s
--   go (x,k)
--     | k < 0 = False
--     | x > mx = k == 0
--     | sa!x == '(' = go (x+1,k+1)
--     | sa!x == ')' = go (x+1,k-1)
--     | sa!x == '?' = go (x+1,k+1) || go (x+1,k-1)

isWF s = go (0,0) where
  !mx = length s-1
  memo = listArray ((0,-1),(mx+1,mx+1)) $ fmap go $ range ((0,-1),(mx+1,mx+1))
  !sa = listArray (0,mx) s
  go (x,k)
    | k < 0 = False
    | x > mx = k == 0
    | sa!x == '(' = memo ! (x+1,k+1)
    | sa!x == ')' = memo ! (x+1,k-1)
    | sa!x == '?' = memo ! (x+1,k+1) || memo ! (x+1,k-1)