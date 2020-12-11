import Data.Char

type Pair = (Int,Int)

lookupRM rm = case toUpper rm of
  'I' -> 1
  'V' -> 5
  'X' -> 10
  'L' -> 50
  'C' -> 100
  'D' -> 500
  'M' -> 1000

tokenizeRM :: String -> [Pair]
tokenizeRM = tokenize . map lookupRM . reverse 
  where 
    tokenize [] = []
    tokenize [x] = [(0,x)]
    tokenize (x:y:xs) = tokenize xs ++ [(y,x)]

runRM :: String -> Int
runRM = foldl (\acc pair -> acc + calcPair pair) 0 . tokenizeRM 
  where calcPair (x,y) = if x < y then y - x else x + y