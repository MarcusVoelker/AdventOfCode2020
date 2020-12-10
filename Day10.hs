module Day10 where

import Data.List

loadData :: IO [Int]
loadData = do
  dat <- readFile "Day10.dat"
  return $ (map read . lines) dat

diffList :: [Int] -> [Int]
diffList dat = (\x -> zipWith (-) (tail x) x ++ [3]) $ sort (0 : dat)

oneSeqs :: [Int] -> [Int]
oneSeqs xs = map length $ filter ((== 1) . head) $ group xs

part1 :: IO ()
part1 = do
  dat <- loadData
  print $ (\y -> length (filter (== 1) y) * length (filter (== 3) y)) $ diffList dat

count :: Int -> Int
count 1 = 1
count 2 = 2
count 3 = 4
count 4 = 7

part2 :: IO ()
part2 = do
  dat <- loadData
  let dl = diffList dat
  print $ product $ map count $ oneSeqs $ dl

test :: [Int]
test =
  [ 16,
    10,
    15,
    5,
    1,
    11,
    7,
    19,
    6,
    12,
    4
  ]