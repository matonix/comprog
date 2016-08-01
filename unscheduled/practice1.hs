import Control.Applicative

main = do
  a <- readLn
  [b,c] <- map read . words <$> getLine
  s <- getLine
  putStrLn $ show (a + b + c) ++ " " ++ s