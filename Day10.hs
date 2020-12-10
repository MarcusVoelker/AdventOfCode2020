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

c :: Int -> Int -> Int
c n k = foldr (\i r -> div (r * (n - i + 1)) i) 1 [1 .. k]

count :: Int -> Int
count x = 2 ^ (x - 1) - sum [c (x - 1) k | k <- [0 .. div (x - 1) 3 - 1]]

part2 :: IO ()
part2 = do
  dat <- loadData
  let dl = diffList dat
  print $ product $ map count $ oneSeqs dl