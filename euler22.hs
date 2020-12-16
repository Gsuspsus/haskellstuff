import Data.Char
import Data.List
import Data.List.Split

names :: IO [(Int, String)]
names = zip [1 ..] . sort . wordsBy (== '\"') . concat . splitOn "," <$> readFile "names.txt"

getAlphaPosition :: Char -> Int
getAlphaPosition = subtract 96 . ord . toLower

letterSum :: String -> Int
letterSum = sum . map getAlphaPosition

getNameScore :: (Int, String) -> Int
getNameScore (p, n) = p * letterSum n

main :: IO ()
main = do
  names' <- names
  let scores = map getNameScore names'
  print (sum scores)