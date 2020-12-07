module Day4 where

import qualified Data.Map as M

type Passport = M.Map String String

splitSpace :: [String] -> [[String]]
splitSpace [] = [[]]
splitSpace ("" : xs) = [] : splitSpace xs
splitSpace (x : xs) = (\(y : ys) -> (x : y) : ys) $ splitSpace xs

buildPassport :: String -> Passport
buildPassport s = M.fromList $ map ((\(a, b) -> (a, tail b)) . break (== ':')) $ words s

loadData :: IO [Passport]
loadData = do
  dat <- readFile "Day4.dat"
  let passes = map (buildPassport . unwords) $ splitSpace $ lines dat
  return passes

isComplete :: Passport -> Bool
isComplete p = all (`M.member` p) $ words "byr iyr eyr hgt hcl ecl pid"

byr :: Passport -> Integer
byr p = read (p M.! "byr")

iyr :: Passport -> Integer
iyr p = read (p M.! "iyr")

eyr :: Passport -> Integer
eyr p = read (p M.! "eyr")

hgt :: Passport -> (Integer, String)
hgt p = head $ reads (p M.! "hgt")

hcl :: Passport -> String
hcl p =
  let code = p M.! "hcl"
   in if head code == '#' && all (`elem` (['0' .. '9'] ++ ['a' .. 'f'])) (tail code)
        then code
        else ""

ecl :: Passport -> String
ecl p = if (p M.! "ecl") `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] then p M.! "ecl" else ""

pid :: Passport -> Integer
pid p = if length (p M.! "pid") == 9 then read (p M.! "pid") else -1

isValid :: Passport -> Bool
isValid p =
  1920 <= byr p && byr p <= 2020
    && 2010 <= iyr p
    && iyr p <= 2020
    && 2020 <= eyr p
    && eyr p <= 2030
    && ((snd (hgt p) == "cm" && 150 <= fst (hgt p) && fst (hgt p) <= 193) || (snd (hgt p) == "in" && 59 <= fst (hgt p) && fst (hgt p) <= 76))
    && hcl p /= ""
    && ecl p /= ""
    && pid p /= -1

part1 :: IO ()
part1 = do
  dat <- loadData
  print $ length $ filter isComplete dat

part2 :: IO ()
part2 = do
  dat <- loadData
  print $ length $ filter (\p -> isComplete p && isValid p) dat