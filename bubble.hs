import CheckSum
import Data.List

bubble :: [Int] -> [Int]
bubble [] = []
bubble [x] = [x]
bubble (x : y : xs)
  | x < y = x : bubble (y : xs)
  | otherwise = y : bubble (x : xs)