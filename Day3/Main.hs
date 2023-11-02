import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Text as T

main :: IO ()
main = do  
    mountain <- lines <$> readFile "input.txt"
    putStrLn (show . foldl (*) 1 . map (\slope -> countTrees slope mountain) $ slopes)

slopes :: [(Int, Int)]
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

countTrees :: (Int, Int) -> [String] -> Int
countTrees (dx, dy) = snd . foldl (\(x, n) line -> ((x+dx) `mod` (length line), (+n) . boolToInt . (=='#') $ (line !! x))) (0, 0) . onlyNth dy

onlyNth :: Int -> [a] -> [a]
onlyNth n = map snd . filter (\(i, _) -> i `mod` n == 0) . zip [0..]

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0
