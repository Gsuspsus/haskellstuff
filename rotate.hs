rotate n str = let (first, second) = splitAt pos str in second ++ first
  where
    pos = if n < 0 then length str + n else n

main :: IO ()
main = do
    let (n,str) = words <$> getLine
    