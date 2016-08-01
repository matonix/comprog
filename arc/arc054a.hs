import Control.Applicative
main = do
  [l,x,y,s,d] <- map read . words <$> getLine :: IO [Int]
  print $ time ((d-s)`mod`l) (x+y) `minplus` time ((s-d)`mod`l) (x-y) where
    time l v = fromIntegral l / fromIntegral v
    minplus a b | a<=0 = b | b<=0 = a | otherwise = min a b