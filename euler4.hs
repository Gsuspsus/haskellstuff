type Product = Integer

isPalindrome :: String -> Bool
isPalindrome x = x == reverse x

palindromesFrom :: Integer -> [(Integer,Integer,Product)]
palindromesFrom 0 = []
palindromesFrom n = makeList [n,n-1..1] [n,n-1..1]
    where
        makeList (1:_) (1:_) = []
        makeList (x:xs) (y:ys) = (x,y,x*y) : makeList xs ys 