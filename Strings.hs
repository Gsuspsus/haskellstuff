findLongestEqual :: String -> String -> String
findLongestEqual [] _ = []
findLongestEqual _ [] = []
findLongestEqual (x:xs) (y:ys) = if x == y then x : findLongestEqual xs ys else []