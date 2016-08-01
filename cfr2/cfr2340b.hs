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

myread = read :: String -> Int
main = do
  _ <- getLine
  ns <- map myread.words <$> getLine
  print $ solve ns

solve :: [Int] -> Int
solve ns = if sum ns == 0 then 0
  else (product . map (\xs->length xs+1) . filter (\xs->head xs==0) . group . cut) ns
    where
      cut = dropWhileEnd (==0) . dropWhile (==0)
