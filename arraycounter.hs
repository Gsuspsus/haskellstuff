import Data.List

countArray :: [Int] -> [Int]
countArray = map length . group . sort

main :: IO ()
main = do
  
  counted <- countArray . map read . words <$> getLine 
  mapM_ (putStr . (++ " ") . show) counted