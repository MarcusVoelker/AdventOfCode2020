module Day6 where

import qualified Data.Set as S

splitSpace :: [String] -> [[String]]
splitSpace [] = [[]]
splitSpace ("" : xs) = [] : splitSpace xs
splitSpace (x : xs) = (\(y : ys) -> (x : y) : ys) $ splitSpace xs

loadData :: IO [[String]]
loadData = do
  dat <- readFile "Day6.dat"
  return $ splitSpace $ lines dat

part1 :: IO ()
part1 = do
  dat <- loadData
  print $ sum $ map (length . foldr1 S.union . map S.fromList) dat

part2 :: IO ()
part2 = do
  dat <- loadData
  print $ sum $ map (length . foldr1 S.intersection . map S.fromList) dat