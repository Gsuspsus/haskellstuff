sumOfSquares :: Integer -> Integer
sumOfSquares n = sum $ map (^2) [1..n]

squareSum :: Integer -> Integer
squareSum n = (sum [0..n])^2

sumSquareDiff :: Integer -> Integer
sumSquareDiff n = squareSum n - sumOfSquares n