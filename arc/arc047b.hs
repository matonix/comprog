import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Array
import           Data.Tuple
import           Data.Int
import           Data.Char
-- import           Data.Function
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict  (IntMap)
-- import qualified Data.IntMap.Strict  as IntMap

type Point = (Int,Int)

myShow = show :: Int -> String 
myRead = read :: String -> Int
printPoint = \(x,y) -> putStrLn (myShow x ++ " " ++ myShow y)

main = do
  n <- readLn
  [x,y] <- transpose . fmap (fmap myRead.words) <$> replicateM n getLine
  printPoint $ solve (zip x y)

solve :: [Point] -> Point
solve (a:b:c:_) = circum a b c 

man :: Point -> Point -> Int
man (a,b) (c,d) = abs(a-c)+abs(b-d)

len :: Point -> Point -> Double
len (a,b) (c,d) = sqrt $ realToFrac ((c-a)^2+(d-b)^2)

--https://ja.wikipedia.org/wiki/%E5%A4%96%E6%8E%A5%E5%86%86
circum :: Point -> Point -> Point -> Point
circum a@(ax,ay) b@(bx,by) c@(cx,cy) =
  (ceiling(ak*(realToFrac ax) + bk*(realToFrac bx) + ck*(realToFrac cx)),
  truncate(ak*(realToFrac ay) + bk*(realToFrac by) + ck*(realToFrac cy)))
  where
    aa = (len b c)^2
    bb = (len c a)^2
    cc = (len a b)^2
    ss = aa*(bb+cc-aa)+bb*(cc+aa-bb)+cc*(aa+bb-cc)
    ak = aa*(bb+cc-aa)/ss
    bk = bb*(cc+aa-bb)/ss
    ck = cc*(aa+bb-cc)/ss