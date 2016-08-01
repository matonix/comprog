-- mod modules --

modNum = 1000000007

modd a = a `mod` modNum

(+%) a b = modd (modd a + modd b)
infixl 6 +%

(-%) a b = modd (modd a - modd b + modNum)
infixl 6 -%

-- usable on Int64
(*%) a b = modd (modd a * modd b)
infixl 7 *%

-- usable only if modNum is prime
(/%) a b = a *% powerMod b (modNum-2) modNum where
  -- SICP Power iteration
  power :: Integral a => (t -> t -> t) -> t -> a -> t -> t
  power _ _ 0 e = e
  power f a n e = power f (f a a) (div n 2) (if odd n then f a e else e)
  powerMod a n m = power (*%) a n 1
infixl 7 /%