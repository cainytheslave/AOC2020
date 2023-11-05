import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord

main :: IO ()
main = do  
    adapters <- map (read :: String -> Int) . words <$> readFile "input.txt"
    putStrLn (show . waysToCombine $ adapters)

-- Part1 solution (not working for some reason)
oneJumpsTimesThreeJumps :: [Int] -> Int
oneJumpsTimesThreeJumps xs = uncurry (*)
                             . (\[a,b] -> (length a + 1, length b + 1))
                             . List.group
                             . List.sort
                             . map (\(a,b) -> b - a)
                             . flip zip (tail xs) $ xs

-- Part2 solution
waysToCombine :: [Int] -> Int
waysToCombine xs = waysToCombine' IntMap.empty (List.sortOn Ord.Down $ 0:xs)

waysToCombine' :: IntMap.IntMap Int -> [Int] -> Int
waysToCombine' memo (x:xs)
  | null xs = possibilities
  | otherwise = waysToCombine' (IntMap.insert x possibilities memo) xs
  where possibilities = max 1 $ foldl (+) 0 . IntMap.filterWithKey (\k _ -> (k - x) <= 3) $ memo
  
