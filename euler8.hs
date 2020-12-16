import Data.Char
import Data.List

type Length = Int

number :: IO String
number = concat . lines <$> readFile "euler8.txt"

prodS :: String -> Int
prodS = product . map digitToInt

greatestSequence :: String -> Length -> String
greatestSequence s n = loop (take n s) (drop n s) n
  where
    loop seq [] _ = seq
    loop seq s n =
      let nseq = take n s 
       in loop (maxProd [seq,nseq]) (drop 1 s) n 
    maxProd = maximumBy (\x y -> prodS x `compare` prodS y) 