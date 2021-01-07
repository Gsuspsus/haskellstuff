import Data.Char

isPalindrome :: String -> Bool
isPalindrome = (\x -> x == reverse x) . filter isAlpha . map toLower 

main :: IO ()
main = do
  lines <- lines <$> getContents
  mapM_ (\x -> if isPalindrome x then putStr "Y " else putStr "N ") lines