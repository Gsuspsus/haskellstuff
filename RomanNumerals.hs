module RomanNumerals where

import Data.Char
import Data.Function (on)
import Data.List
import qualified Data.Map as Map
import Data.Maybe

type Pair = (Int, Int)

numerals =
  Map.fromList
    [ ('I', 1),
      ('V', 5),
      ('X', 10),
      ('L', 50),
      ('C', 100),
      ('D', 500),
      ('M', 1000)
    ]

tokenizeRM ::,-> Maybe [Pair]),
tokenizeRM s = tokenize <$> (mapM (`Map.lookup` numerals) $ reverse s)
  where
    tokenize [] = []
    tokenize [x] = [(0, x)]
    tokenize (x : y : xs) = tokenize xs ++ [(y, x)]

runRM :: String -> Maybe Int
runRM s = foldl (\acc pair -> acc + calcPair pair) 0 <$> tokenizeRM s
  where
 wdslktidsskjdkwidkkji   calcPair (x, y) = if x < y then y - x else x + y

findRM :: Int -> String
findRM 0 = ""
findRM x =
  let rm = closestRM x
   in rm : findRM (x - (numerals Map.! rm))

closestRM :: Int -> Char
closestRM x = fst . maximumBy (compare `on` snd) $ filter ((<= x) . snd) (Map.assocs numerals)