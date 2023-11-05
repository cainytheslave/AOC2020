import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Char as Char

type Rule = (String, [(Int, String)])
type Rulemap = Map.Map String [(Int, String)]

main :: IO ()
main = do  
    rules <- Map.fromList . map parseRule . lines <$> readFile "input.txt"
    putStrLn (show $ (canStoreN "shiny gold" rules))

canStoreNof :: String -> Rulemap -> String -> Int
canStoreNof s rules key = sum . map (\(num, col) -> if col == s then num else (canStoreNof s rules col)) $ rules Map.! key

canStoreN :: String -> Rulemap -> Int
canStoreN key rules = sum . map (\(num, col) -> num + num * (canStoreN col rules)) $ rules Map.! key

parseRule :: String -> Rule
parseRule s = let [color, contents] = Split.splitOn " bags contain " $ s
                  contentList = parseContents $ contents
              in (color, contentList)

parseContents :: String -> [(Int, String)]
parseContents s
  | s == "no other bags." = []
  | otherwise = map parseBag $ bags
  where bags = Split.splitOn ", " s

parseBag :: String -> (Int, String)
parseBag s = let bag = filter (\w -> w /= "bags" && w /= "bag") . words . filter (not . Char.isPunctuation) $ s
             in (read . head $ bag, unwords . tail $ bag)
        

                 
