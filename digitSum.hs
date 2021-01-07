import Data.Char

sumDigits :: Int -> Int
sumDigits = sum . map digitToInt . show 

main :: IO ()
main = do
  lines <- lines <$> getContents
  let numbers = map ((\[a,b,c] -> a*b+c) . (map read . words)) lines 
  mapM_ (putStr . (++" ") . show . sumDigits) numbers
  putStrLn ""