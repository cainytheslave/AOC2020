import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import Data.List.Split
import qualified Data.Text as T
import Data.Char

main :: IO ()
main = do  
    pps <- map (M.fromList . map ((\[a, b] -> (a, b)) . splitOn ":") . words) . splitOn "\n\n" <$> readFile "input.txt"
    putStrLn (show . length . filter id . map validatePPStrict $ pps)

validatePP :: M.Map String String -> Bool
validatePP m = null . filter (\x -> not (M.member x m)) $ ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validatePPStrict :: M.Map String String -> Bool
validatePPStrict m = validatePP m && (valBYR $ m M.! "byr") && (valIYR $ m M.! "iyr") && (valEYR $ m M.! "eyr") && (valHGT $ m M.! "hgt") && (valHCL $ m M.! "hcl") && (valECL $ m M.! "ecl") && (valPID $ m M.! "pid")
  where valBYR s = let n = read s in (n >= 1920 && n <= 2002)
        valIYR s = let n = read s in (n >= 2010 && n <= 2020)
        valEYR s = let n = read s in (n >= 2020 && n <= 2030)
        valHGT s = let (h, unit) = span isDigit $ s in if unit == "cm" then (read h) >= 150 && (read h) <= 193 else (read h) >= 59 && (read h) <= 76
        valHCL (x:xs) = x == '#' && (all isHexDigit $ xs)
        valECL s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        valPID s = length s == 9 && (all isNumber $ s)
