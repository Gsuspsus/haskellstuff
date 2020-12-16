import Data.List

collatz :: Int -> Int
collatz n
  | even n = n `div` 2
  | otherwise = n * 3 + 1

collatzChain :: Int -> [Int]
collatzChain 1 = [1]
collatzChain n = n : collatzChain (collatz n)

