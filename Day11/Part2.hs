import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Vector as Vector
import Data.Function

type Matrix = Vector.Vector (Vector.Vector Char)

main :: IO ()
main = do  
    seats <- toMatrix . lines <$> readFile "input.txt"
    putStrLn (show . countSeatsTaken . stepUntilStatic $ seats)

toMatrix :: [[Char]] -> Matrix
toMatrix = (Vector.map Vector.fromList) . Vector.fromList

fromMatrix :: Matrix -> [[Char]]
fromMatrix = Vector.toList . (Vector.map Vector.toList)

matrixToString :: Matrix -> String
matrixToString = unlines . fromMatrix

countSeatsTaken :: Matrix -> Int
countSeatsTaken = sum . Vector.toList . Vector.map (Vector.length . Vector.filter(=='#'))

stepUntilStatic :: Matrix -> Matrix
stepUntilStatic m = let next = step m
                     in if(m == next) then m else stepUntilStatic next

steps :: Int -> Matrix -> Matrix
steps n m
    | n >= 0    = steps (n-1) $ step m
    | otherwise = m

step :: Matrix -> Matrix
step m = Vector.imap (\y -> Vector.imap (\x c -> evolve (x, y) c m)) $ m

evolve :: (Int, Int) -> Char -> Matrix -> Char
evolve (x, y) state m
    | state == '.' = state
    | inUse == 0 = '#'
    | inUse >= 5 = 'L'
    | otherwise  = state
    where directions = [(dx, dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0,0)]
          inUse = length . filter (=='#') . map (\dir -> firstInDirection (x, y) dir m) $ directions

evolve' :: (Int, Int) -> Char -> Matrix -> Int
evolve' (x, y) state m = inUse
    where directions = [(dx, dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0,0)]
          inUse = length . filter (=='#') . map (\dir -> firstInDirection (x, y) dir m) $ directions


firstInDirection :: (Int, Int) -> (Int, Int) -> Matrix -> Char
firstInDirection pos (dx, dy) m = head . dropWhile (=='.') . map (getState m) . tail $ iterate (\(x, y) -> (x+dx, y+dy)) pos

getState :: Matrix -> (Int, Int) -> Char
getState m (x, y) = Maybe.fromMaybe 'X' $ (m Vector.!? y) >>= (\row -> row Vector.!? x)

