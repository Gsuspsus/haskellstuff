{-
  20 21 22 23 24
  19  6  7  8 9
  18  5  0  1 10
  17  4  3  2 11
  16 15 14 13 12
-}

chainLenghts :: Int -> [Int]
chainLenghts n = loop n 2 1
  where
    loop 0 _ _ = []
    loop n len times =
      let len' = if times `mod` 3 == 0 then succ len else len
       in len' : loop (pred n) len' (succ times)

createChain :: [Int] -> [[Int]]
createChain xs = loop n xs (chainLenghts n)
  where
    n = length xs
    loop _ [] _ = []
    loop 0 _ _  = []
    loop n xs (l : ls) = let (y, ys) = splitAt l xs in y : loop (pred n) ys ls