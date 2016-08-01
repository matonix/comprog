import Control.Applicative
import Control.Monad

main = do
  [l, h] <- fmap (read :: String -> Int) . words <$> getLine
  n <- readLn
  a <- replicateM n readLn
  mapM_ (print . solve l h) a

solve :: Int -> Int -> Int -> Int
solve l h v = if v < l then l - v else if v <= h then 0 else -1
