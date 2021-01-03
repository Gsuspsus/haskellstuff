import Data.Char

section :: String -> (Int, [String])
section xs = let ([n], rest) = (splitAt 1 $ lines xs) in (read n,rest)

cleanExpr :: String -> String
cleanExpr = filter (not . isSpace)

parseOp :: Char -> (Int -> Int -> Int)
parseOp op = case op of 
                '+' -> (+)
                '-' -> (-)
                '/' -> div
                '*' -> (*)
                '%' -> mod

run :: Int -> [String] -> Int 
run = foldl (\acc (op:n) -> parseOp op acc (read n) )

main = do
    (init,prog) <- section . unlines . lines <$> readFile "calcProg.txt"
    let result= run init (map cleanExpr prog)    
    print result