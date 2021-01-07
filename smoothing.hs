smooth :: [Double] -> [Double]
smooth [] = []
smooth (x:xs) = x : loop xs
  where
    loop xs = (sum $ take 3 xs)/3 : loop (tail xs)