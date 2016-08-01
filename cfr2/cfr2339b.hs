import           Control.Applicative
import           Control.Monad
import           Data.List
import qualified Data.ByteString.Char8 as C

-- myShow = show :: Int -> String 
main = do
  _ <- C.getLine
  xs <- C.getLine
  print $ solve xs

solve :: C.ByteString -> Integer
solve = product . map ((\(Just x) -> fst x) . C.readInteger) . C.words
