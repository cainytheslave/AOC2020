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

type Position = (Int, Int, Char)
type Waypoint = (Int, Int)

main :: IO ()
main = do  
    instr <- map (\(x:xs) -> (x, read xs) :: (Char, Int)) . lines <$> readFile "input.txt"
    putStrLn (show $ (\(x,y,_) -> abs x + abs y) . navigate (10, -1) (0,0,'E') $ instr)

navigate2 :: Waypoint -> Position -> [(Char, Int)] -> Position
navigate2 waypoint pos [] = pos
navigate2 (dx, dy) (x, y, dir) ((cmd, arg):rest)
  | cmd == 'N' = navigate2 (dx, dy-arg)(x, y, dir) rest
  | cmd == 'E' = navigate2 (dx-arg, dy) (x, y, dir) rest
  | cmd == 'S' = navigate2 (dx, dy+arg) (x, y, dir) rest
  | cmd == 'W' = navigate2 (dx+arg, dy)(x, y, dir) rest
  | cmd == 'L' = navigate2 () (x, y, newRotation (-)) rest
  | cmd == 'R' = navigate2 (x, y, newRotation (+)) rest
  | cmd == 'F' = navigate2 (x, y, dir) $ (dir, arg):rest
  where newRotation f = "NESW" List.!! ((f (Maybe.fromJust . List.elemIndex dir $ "NESW") (arg `div` 90)) `mod` 4)

navigate :: Position -> [(Char, Int)] -> Position
navigate (x, y, dir) [] = (x, y, dir)
navigate (x, y, dir) ((cmd, arg):rest)
  | cmd == 'N' = navigate (x, y-arg, dir) rest
  | cmd == 'E' = navigate (x-arg, y, dir) rest
  | cmd == 'S' = navigate (x, y+arg, dir) rest
  | cmd == 'W' = navigate (x+arg, y, dir) rest
  | cmd == 'L' = navigate (x, y, newRotation (-)) rest
  | cmd == 'R' = navigate (x, y, newRotation (+)) rest
  | cmd == 'F' = navigate (x, y, dir) $ (dir, arg):rest
  where newRotation f = "NESW" List.!! ((f (Maybe.fromJust . List.elemIndex dir $ "NESW") (arg `div` 90)) `mod` 4)



