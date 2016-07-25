import Data.List

main = do
  _ <- getLine
  ls <- map (read::String -> Int) . words <$> getLine
  print $ solve ls

solve ls = smaller $ sort ls where
  smaller [] = 0
  smaller (a:b:xs) = min a b + smaller xs
