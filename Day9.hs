module Day9 where

import Data.Maybe

loadData :: IO [Int]
loadData = do
  dat <- readFile "Day9.dat"
  return $ (map read . lines) dat

valid :: [Int] -> Int -> Bool
valid xs y = y `elem` [a + b | a <- xs, b <- xs, a /= b]

slices :: [Int] -> [([Int], Int)]
slices xs = slices' (take 25 xs) (drop 25 xs)
  where
    slices' _ [] = []
    slices' (w : ws) (x : xs) = (w : ws, x) : slices' (ws ++ [x]) xs

p1 :: [Int] -> Int
p1 dat = snd $ head $ filter (not . uncurry valid) $ slices dat

sumRange :: Int -> [Int] -> [Maybe [Int]]
sumRange s (x : xs) = sumRange' s (x : xs) : sumRange s xs
  where
    sumRange' s [] = Nothing
    sumRange' s (x : xs)
      | s == x = Just [s]
      | s < x = Nothing
      | s > x = (x :) <$> sumRange' (s - x) xs

part1 :: IO ()
part1 = do
  dat <- loadData
  print $ p1 dat

part2 :: IO ()
part2 = do
  dat <- loadData
  let t = p1 dat
  print $ (\xs -> maximum xs + minimum xs) $head $ catMaybes $ sumRange t dat
