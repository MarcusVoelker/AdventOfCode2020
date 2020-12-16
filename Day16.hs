module Day16 where

import Data.List
import qualified Data.Map as M

data Rule = Rule {name :: String, low1 :: Int, high1 :: Int, low2 :: Int, high2 :: Int} deriving (Show)

type Ruleset = M.Map String Rule

dSpan :: Int -> ([a] -> b) -> (a -> Bool) -> [a] -> (b, [a])
dSpan n f p xs = (\(a, b) -> (f a, drop n b)) $ span p xs

parseRule :: String -> Rule
parseRule s =
  let (n, r1) = dSpan 2 id (/= ':') s
      (l1, r2) = dSpan 1 read (/= '-') r1
      (h1, r3) = dSpan 4 read (/= ' ') r2
      (l2, r4) = dSpan 1 read (/= '-') r3
      h2 = read r4
   in Rule n l1 h1 l2 h2

loadData :: IO (Ruleset, [Int], [[Int]])
loadData = do
  dat <- readFile "Day16.dat"
  let ldat = lines dat
  let (rs, r) = dSpan 2 (map parseRule) (/= []) ldat
  let t = read $ '[' : head r ++ "]"
  let r' = drop 3 r
  return (M.fromList $ map (\r -> (name r, r)) rs, t, map (\l -> read $ '[' : l ++ "]") r')

fits :: Int -> Rule -> Bool
fits i r = (low1 r <= i && i <= high1 r) || (low2 r <= i && i <= high2 r)

scoreTicket :: Ruleset -> [Int] -> Int
scoreTicket m t =
  let rs = M.elems m
   in sum $ filter (\i -> all (not . (i `fits`)) rs) t

legalTicket :: Ruleset -> [Int] -> Bool
legalTicket m t =
  let rs = M.elems m
   in all (\i -> any (i `fits`) rs) t

part1 :: IO ()
part1 = do
  (rs, _, ts) <- loadData
  print $ sum $ map (scoreTicket rs) ts

fieldCandidates :: Ruleset -> [Int] -> [String]
fieldCandidates rs is = M.keys $ M.filter (\r -> all (`fits` r) is) rs

stepOrder :: (M.Map Int String, M.Map Int [String]) -> (M.Map Int String, M.Map Int [String])
stepOrder (o, cs) =
  let ss = M.filter ((== 1) . length) cs
      o' = M.union (M.map head ss) o
      cs' = M.filter (not . null) $ M.map (\\ concat (M.elems ss)) cs
   in (o', cs')

findOrder :: M.Map Int [String] -> M.Map Int String
findOrder cs = fst $ head $ dropWhile (not . null . snd) $ iterate stepOrder (M.empty, cs)

part2 :: IO ()
part2 = do
  (rs, t, ts) <- loadData
  let legals = transpose $ filter (legalTicket rs) ts
  let o = findOrder $ M.fromList $ zip [0 ..] $ map (fieldCandidates rs) legals
  print $ product $ map (t !!) $ M.keys $ M.filter (\s -> take 9 s == "departure") o