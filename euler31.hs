import Data.List
import Data.Char

isPan :: Int -> Bool
isPan n = isNonRecurring n && containsDigits n
  where 
    isNonRecurring n = length (nub n') == length n'
    containsDigits n = all ((`elem` n') . intToDigit) [1..(length n')]
    n' = show n
  
isPanIdentifier :: Int -> Int -> Bool 
isPanIdentifier a b = isPan (read (show a ++ show b ++ show (a*b)))

