module Day21 where

import Data.List
import Data.Ord

parseList :: String -> ([String], [String])
parseList s =
  let (i, d) = span (/= '(') s
   in (words i, tail $ words $ filter (/= ',') $ tail (init d))

loadData :: IO [([String], [String])]
loadData = do
  dat <- readFile "Day21.dat"
  return $ map parseList $ lines dat

cands :: [([String], [String])] -> [[String]]
cands dat =
  let als = nub (dat >>= snd)
   in map (\a -> foldr1 intersect $ map fst $ filter (\d -> a `elem` snd d) dat) als

part1 :: IO ()
part1 = do
  dat <- loadData
  let c = nub $ concat $ cands dat
  print $ length $ filter (`notElem` c) $ dat >>= fst

step :: [([String], String)] -> [([String], String)]
step xs =
  let singles = filter (\x -> length (fst x) == 1) xs
   in map (\(is, a) -> (if length is == 1 then is else is \\ map (concat . fst) singles, a)) xs

findFix :: (Eq a) => (a -> a) -> a -> a
findFix f i
  | f i == i = i
  | otherwise = findFix f (f i)

part2 :: IO ()
part2 = do
  dat <- loadData
  let als = nub (dat >>= snd)
  let c = cands dat
  print $ intercalate "," $ concatMap fst $ findFix step $ sortBy (comparing snd) $ zip c als
