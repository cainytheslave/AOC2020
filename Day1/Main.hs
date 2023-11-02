import qualified Data.Set as S
import qualified Data.List as L

main :: IO ()
main = do  
    numbers <- map read . lines <$> readFile "input.txt"
    putStrLn (show . (\(a,b,c) -> a*b*c) . findThreeSum 2020 $ numbers)  


findSum :: (Num a, Eq a) => a -> [a] -> (a, a)
findSum _ [] = (-1, -1)
findSum n (x:xs) = if (n-x) `elem` xs then (x, (n-x)) else findSum n xs

findThreeSum :: (Num a, Eq a) => a -> [a] -> (a, a, a)
findThreeSum n xs = head $ [(a,b,c) | a <- xs, b <- xs, c <- xs, a+b+c == n] 
