import Control.Monad
import System.Console.ANSI
import System.Random

newtype Bleep = Bleep (Char, (Int, Int))

bleepColors = [Yellow, Blue, Red, Green]

showBleep :: Bleep -> IO ()
showBleep (Bleep (c, (x, y))) = do
  setCursorPosition x y
  setSGR [SetColor Foreground Vivid Red]
  putStr $ show c

generateBleeps :: StdGen -> Int -> [Bleep]
generateBleeps g n = map (\(c,x,y) -> Bleep (c, (x,y))) zipped 
    where 
        zipped = zip3 cs xs ys
        cs = map toEnum $ take n $ randomRs (97,122) g 
        xs = take n $ randomRs (10,40) g
        ys = take n $ randomRs (10,80) g

main :: IO ()
main = do
  clearScreen
  g <- getStdGen
  let bleeps = generateBleeps g 10
  mapM_ showBleep bleeps
  setSGR [Reset]