import Control.Monad
import qualified Data.Bifunctor as BF
import Data.List
import Data.Maybe
import Data.Ord
import System.Console.ANSI

type Position = (Int, Int)
type Length = Int
type Grid = [[Int]]
type Strip = [Int]

data Cursor = Cursor
  { pos :: Position,
    len :: Length
  }

data Direction = L | R | U | D | Diag deriving (Show, Enum)

selectStrip :: Grid -> Cursor -> Direction -> Strip
selectStrip g c dir = fromMaybe [] maybeStrip
  where
    maybeStrip = mapM (g `at`) $ lineToPositions c dir

lineToPositions :: Cursor -> Direction -> [Position]
lineToPositions (Cursor p l) dir = map createPos [0 .. l -1]
  where
    createPos n = case dir of
      L -> BF.second (`minus` n) p
      R -> BF.second (+ n) p
      U -> BF.second (`minus` n) p
      D -> BF.first (+ n) p
    minus = (-)

parseGrid :: String -> Grid
parseGrid = map (map read . words) . lines

main :: IO ()
main = do
  grid <- parseGrid <$> readFile "euler11grid.txt"
  let cursor = Cursor (5, 5) 5
  printGrid grid
  let strips = map (selectStrip grid cursor) [L, R, U, D]
  mapM_ printStrip strips

searchGrid :: Grid -> Cursor -> (Strip -> Bool) -> Maybe [Strip]
searchGrid g c p = 

searchUnderCursor :: Grid -> Cursor -> (Strip -> Bool) -> Maybe Strip
searchUnderCursor g c p = case filtered of
  (x : _) -> Just x
  [] -> Nothing 
  where
    filtered = filter p $ map (selectStrip g c) [L, R, U, D]

testStrip :: Strip -> (Strip -> Bool) -> Bool
testStrip s p = p s

-- IO --

printGrid :: Grid -> IO ()
printGrid = mapM_ printStrip

printStrip :: [Int] -> IO ()
printStrip row = do
  printAlligned row
  putStrLn ""

at :: Grid -> (Int, Int) -> Maybe Int
at grid (y, x) = if outOfVertBounds || outOfHoriBounds then Nothing else Just $ grid !! y !! x
  where
    outOfVertBounds = y < 0 || y > length grid
    outOfHoriBounds = x < 0 || x > length (head grid)

printAlligned :: [Int] -> IO ()
printAlligned row = mapM_ putStr $ intersperse " " alligned
  where
    alligned = map allign row'
    allign x = if length x < maxLength row then lead x else x
    row' = map show row
    lead x = replicate (maxLength row - length x) '0' ++ x

maxLength :: [Int] -> Int
maxLength = strlen . maximumBy (\x y -> strlen x `compare` strlen y)
  where
    strlen = length . show