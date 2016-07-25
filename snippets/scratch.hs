import Control.Applicative
import Control.Monad
import Data.List

type I = Int

main :: IO ()
main = do
  m <- readi <$> getLine
  ns <- map readi <$> replicateM m getLine
  print $ solve ns

solve :: [I] -> [I]
solve = id

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

readi :: String -> I
readi = read

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)

toTriple :: [a] -> (a, a, a)
toTriple [x, y, z] = (x, y, z)
