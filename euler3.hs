largestPrimeFactor :: Integer -> Maybe Integer
largestPrimeFactor n = computeFactor 2 3 1 n

computeFactor x y d n
  | d == 1 = computeFactor (g x) ((g . g) y) (gcd (abs (x - y)) n) n
  | d == n = Nothing
  | otherwise = Just d
  where
    g x = (x ^ 2 + 1) `mod` n