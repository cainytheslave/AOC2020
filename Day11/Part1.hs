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

type Matrix = Map.Map (Int, Int) Char

main :: IO ()
main = do  
    seats <- toMatrix . lines <$> readFile "input.txt"
    putStrLn (show . Map.size . Map.filter(=='#') . stepUntilStatic $ seats)

toMatrix :: [[Char]] -> Matrix
toMatrix chars = Map.fromList $ [((x, y), c) | (y, row) <- zip [0..] chars, (x, c) <- zip [0..] row]

fromMatrix :: Matrix -> [[Char]]
fromMatrix m = 
  let groupedByRow = List.groupBy ((==) `on` (snd . fst))
                    . List.sortBy (Ord.comparing (snd . fst))
                    $ Map.assocs m
  in map (map snd) groupedByRow

stepUntilStatic :: Matrix -> Matrix
stepUntilStatic m = let next = steps 1 m
                    in if m == next then m else stepUntilStatic next

steps :: Int -> Matrix -> Matrix
steps n m
  | n <= 0 = m
  | otherwise = steps (n-1) $ Map.mapWithKey (\k v -> if v == '.' then '.' else calculateNextState k v m) $ m

calculateNextState :: (Int, Int) -> Char -> Matrix -> Char
calculateNextState (x, y) val m
  | val == '.'  = val
  | occupied == 0 = '#'
  | occupied >= 4 = 'L'
  | otherwise     = val
  where occupied  = length . filter (=='#') . map (\pos -> Map.findWithDefault '.' pos m) $ squaresToCheck     
        squaresToCheck = [(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], (x', y') /= (x, y)]

