import Control.Applicative()


main = do
  [a, d] <- fmap (read :: String -> Int) . words <$> getLine
  print $ solve a d

solve :: Int -> Int -> Int
solve a d =
  if (a + 1) * d >= a * (d + 1)
  then (a + 1) * d
  else a * (d + 1)
