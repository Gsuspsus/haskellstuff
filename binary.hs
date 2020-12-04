import Data.Char

main :: IO ()
main = do
    b <- getLine
    maybe (putStrLn "Invalid Number") print (calculateBinary <$> parseBinary b)

    return ()

parseBinary :: String -> Maybe [Int]
parseBinary b = fmap digitToInt <$> validNumbers b
    where 
        isValid n = if isNumber n && n `elem` ['0','1'] then Just n else Nothing
        validNumbers = mapM isValid

calculateBinary :: [Int] -> Int
calculateBinary b = sum $ zipWith (\a b -> a * 2^b) b [l,l-1..0] 
    where 
        l = length b - 1