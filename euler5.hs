mods :: Integer -> Integer -> [Integer]
mods x y = mod <$> [x] <*> [1..y]

findModFrom :: Integer -> Maybe Integer
findModFrom l = loop 1
    where 
        loop n = if all (==0) $ mods n l then Just n else loop (n+1)