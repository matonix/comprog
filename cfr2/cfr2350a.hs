main = getLine >>= putStrLn . calc . read
calc x = show lo ++ " " ++ show hi where
  hi = x`div`7*2 + min (x`mod`7) 2
  lo = x`div`7*2 + max (x`mod`7-5) 0