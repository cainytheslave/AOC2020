import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Char as Char

main :: IO ()
main = do  
    answers <- map lines . Split.splitOn "\n\n" <$> readFile "input.txt"
    putStrLn (show . sum . map countAll $ answers)

countUnique :: [String] -> Int
countUnique = length . List.nub . concat

countAll :: [String] -> Int
countAll a = let people = length a
             in sum . map (\l -> if length l == people then 1 else 0) . List.group . List.sort . concat $ a
                 
