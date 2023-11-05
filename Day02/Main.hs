import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Text as T

main :: IO ()
main = do  
    passwords <- map words . lines <$> readFile "input.txt"
    putStrLn (show . length . filter (==True) . map validatePassword2 $ passwords)

validatePassword :: [String] -> Bool
validatePassword [range, letter, pw] = let (lower, upper) = (\(x, y) -> (read x, read y)) . splitOn '-' $ range
                                           letter' = head letter
                                           len = length . filter (==letter') $ pw
                                       in len >= lower && len <= upper

validatePassword2 :: [String] -> Bool
validatePassword2 [range, letter, pw] = let (lower, upper) = (\(x, y) -> ((read x) - 1, (read y) - 1)) . splitOn '-' $ range
                                            letter' = head $ letter
                                        in (pw !! lower == letter') /= (pw !! upper == letter')


boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

splitOn :: Char -> String -> (String, String)
splitOn c s = let (l, r) = span (/=c) s
              in (l, tail r) 
