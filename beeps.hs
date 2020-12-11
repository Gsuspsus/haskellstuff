import Control.Concurrent
import Control.Monad
import System.Console.ANSI
import System.IO
import System.Random
import Data.Maybe

data Bleep = Bleep
  { symbol :: Char,
    color :: Color,
    positon :: (Int, Int)
  }

data World = World
  { dimensions :: (Int, Int),
    creatures :: [Creature]
  }

class Creature where
  update :: World -> a


instance Creature Bleep where
  update (Bleep s c (x,y)) (World (h,w) bs) = if x < 0 || x > w then Bleep s c (0,y) else Bleep s c (x+1,y) 

bleepColors = [Yellow, Blue, Red, Green]

showWorld :: World -> IO ()
showWorld = mapM_ showBleep . creatures

showBleep :: Bleep -> IO ()
showBleep (Bleep s c p) = do
  uncurry setCursorPosition p
  setSGR [SetColor Foreground Vivid c]
  putStr $ show s

generateBleeps :: StdGen -> Int -> [Bleep]
generateBleeps _ 0 = []
generateBleeps g n = Bleep s col (x, y) : generateBleeps g' (pred n)
  where
    s = toEnum . fst $ randomR (97, 122) g
    x = fst $ randomR (10, 40) g
    y = fst $ randomR (10, 40) g'
    col = bleepColors !! (fst $ randomR (0, 3) g)
    g' = snd $ split g

updateWorld :: World -> World
updateWorld (World (h, w) b) = World (h,w) $ map updateBleeps b
  where
    update (y, x) = if x > w then (y, 0) else (y, x + 1)
    updateBleeps (Bleep s c p) = Bleep s c (update p)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  clearScreen
  hideCursor
  (h, w) <- liftM (fromMaybe (0,0)) getTerminalSize
  setCursorPosition 0 0
  g <- getStdGen
  let world = World (h, w) $ generateBleeps g 10
  runWorld world

runWorld :: World -> IO ()
runWorld w = do
  showWorld w
  threadDelay 10000
  clearScreen
  runWorld (updateWorld w)