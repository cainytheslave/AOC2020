import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Char as Char

main :: IO ()
main = do  
    rules <- lines <$> readFile "input.txt"
    putStrLn (map parseRule $ rules)

canStore :: String -> 

parseRule :: String -> (String, [(Int, String)])
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
        

                 
