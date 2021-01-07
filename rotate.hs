import Control.Monad

rotate :: Int -> String -> String
rotate n str = let (first, second) = splitAt pos str in second ++ first
  where
    pos = if n < 0 then length str + n else n

main :: IO ()
main = do
  lines <- lines <$> getContents
  let results = map ((\[n,str] -> rotate (read n) str) . words) lines
  mapM_ (\x -> putStr x >> putStr " ") results