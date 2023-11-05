import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe

main :: IO ()
main = do  
    numbers <- map (read :: String -> Int) . words <$> readFile "input.txt"
    putStrLn (show . findContiguousSumOf 138879426 $ numbers)

firstThatDoesntCompose :: [Int] -> Int
firstThatDoesntCompose xs = let (preamble, rest) = List.splitAt(25) xs
                            in if null rest || canCompose (head rest) preamble then firstThatDoesntCompose (drop 1 xs) else (head rest)

findContiguousSumOf :: Int -> [Int] -> [[Int]]
findContiguousSumOf x = map List.sort . filter ((==x) . sum) . map (takeUntil ((>=x) . sum)) . List.tails

canCompose :: Int -> [Int] -> Bool
canCompose x xs = not . null $ [(i, j) | i <- xs, j <- xs, i /= j, i + j == x]

takeUntil :: ([a] -> Bool) -> [a] -> [a]
takeUntil pred xs = go [] xs
  where go accList ys
         | null ys || pred accList = reverse accList
         | length ys == 1          = reverse (head ys:accList)
         | otherwise               = go (head ys:accList) (tail ys)

