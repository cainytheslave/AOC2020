import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import Data.List.Split
import qualified Data.Text as T
import Data.Char

main :: IO ()
main = do  
    tickets <- lines <$> readFile "input.txt"
    putStrLn (show . findMissing . L.sort . map toSeat $ tickets)

findMissing :: [Int] -> Int
findMissing [] = -1
findMissing (p:s:xs) = if p+1 == s then findMissing (s:xs) else (p+1)

toSeat :: String -> Int
toSeat xs = let (rowB, columnB) = splitAt 7 xs
                row = bToInt . map (\x -> if x == 'F' then 0 else 1) $ rowB
                column = bToInt . map (\x -> if x == 'L' then 0 else 1) $ columnB
            in (row * 8 + column)

bToInt :: [Int] -> Int
bToInt = sum . zipWith (*) (iterate (*2) 1) . reverse
