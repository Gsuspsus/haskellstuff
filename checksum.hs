module CheckSum (checksum) where

checksum :: (Integral a) => [a] -> a
checksum = foldl (\acc e -> ((acc + e) * 113) `mod` 10000007) 0

main :: IO ()
main = do
  numbers <- map read . words <$> getLine
  print $ checksum numbers