isPyTriplet :: Int -> Int -> Int -> Bool
isPyTriplet a b c = (a < b && b < c) && a ^ 2 + b ^ 2 == c ^ 2

type Triplet = [Int]

findPyTriplets :: [Int] -> [Triplet]
findPyTriplets xs = loop xs
  where
    loop xs
      | length xs < 3 = []
      | otherwise =
        let chunk@[a, b, c] = take 3 xs
            rest = drop 1 xs
         in if isPyTriplet a b c
              then chunk : loop rest
              else loop rest
