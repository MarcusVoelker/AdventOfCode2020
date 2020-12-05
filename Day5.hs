module Day5 where

conv :: String -> Int
conv = conv' 0
  where
    conv' r "" = r
    conv' r ('F' : xs) = conv' (r * 2) xs
    conv' r ('B' : xs) = conv' (r * 2 + 1) xs
    conv' r ('L' : xs) = conv' (r * 2) xs
    conv' r ('R' : xs) = conv' (r * 2 + 1) xs

loadData :: IO [Int]
loadData = do
  dat <- readFile "Day5.dat"
  return $ map conv $ lines dat

findMissing :: [Int] -> Int
findMissing xs = head $ dropWhile (\x -> x `elem` xs || (x -1) `notElem` xs || (x + 1) `notElem` xs) [1 ..]

part1 :: IO ()
part1 = do
  dat <- loadData
  print $ maximum dat

part2 :: IO ()
part2 = do
  dat <- loadData
  print $ findMissing dat
