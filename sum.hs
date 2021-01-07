sum' :: (Num a) => [a] -> a
sum' = foldl1 (+)

main :: IO ()
main = do
  lines  <- lines <$> getContents
  let numbers = map (map read . words) lines :: [[Int]]
  mapM_ (putStr . (++" ") . show . sum') numbers 