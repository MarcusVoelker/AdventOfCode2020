module Day1 where

import Data.Maybe

part1 :: [Int] -> Int
part1 (x:xs) 
    | any (\y -> x+y == 2020) xs = x*(2020-x)
    | otherwise = part1 xs

part1Gen :: Int -> [Int] -> Maybe Int
part1Gen _ [] = Nothing
part1Gen i (x:xs) 
    | any (\y -> x+y == i) xs = Just (x*(i-x))
    | otherwise = part1Gen i xs

part2 :: [Int] -> Int
part2 (x:xs) = maybe (part2 xs) (x*) $ part1Gen (2020-x) xs

loadData :: IO [Int]
loadData = do
    dat <- readFile "Day1.dat"
    return $ (map read . lines) dat

step1 :: IO ()
step1 = do
    dat <- loadData
    print $ part1 dat

step2 :: IO ()
step2 = do
    dat <- loadData
    print $ part2 dat