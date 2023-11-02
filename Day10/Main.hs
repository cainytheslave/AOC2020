import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe

main :: IO ()
main = do  
    adapters <- List.sort . map (read :: String -> Int) . words <$> readFile "input.txt"
--Part1    putStrLn (show . uncurry (*) . (\[a,b] -> (length a + 1, length b + 1)) . List.group . List.sort . map (\(a,b) -> b - a) . (flip zip (tail adapters)) $ adapters)
    putStrLn (show $ adapters)

