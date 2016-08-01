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
-- import           Data.Array
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
  ns <- map ((\[x,a,b]->(head x,myread a,myread b)).words) <$> replicateM n getLine
  print $ solve n ns

solve :: Int -> [(Char,Int,Int)] -> Int
solve n ns = maximum . map (go (partition (\(x,_,_)->x=='M') ns)) $ [1..366] where
  go !(ms,fs) !d = 2 * min (go2 d ms) (go2 d fs) where
    go2 d rs = length . filter (\(_,a,b) -> a<=d && d<=b) $ rs