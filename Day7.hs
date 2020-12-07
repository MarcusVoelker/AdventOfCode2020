module Day7 where

import qualified Data.Map as M

data Rule = Rule {outer :: String, inner :: [(String, Int)]} deriving (Show)

parseDesc :: [String] -> [(String, Int)]
parseDesc ["no", "other", "bags."] = []
parseDesc [] = []
parseDesc (l : c1 : c2 : _ : s) = (c1 ++ " " ++ c2, read l) : parseDesc s

parseRule :: String -> Rule
parseRule s =
  let w = words s
      o = head w ++ " " ++ w !! 1
      desc = drop 4 w
   in Rule o (parseDesc desc)

loadData :: IO (M.Map String Rule)
loadData = do
  dat <- readFile "Day7.dat"
  return $ M.fromList $ map ((\r -> (outer r, r)) . parseRule) $ lines dat

contains :: M.Map String Rule -> String -> String -> Bool
contains m b c
  | b == c = True
  | any (contains m b . fst) (inner (m M.! c)) = True
  | otherwise = False

containCount :: M.Map String Rule -> String -> Int
containCount m b = sum $ map (\(b', c) -> (containCount m b' + 1) * c) (inner (m M.! b))

part1 :: IO ()
part1 = do
  dat <- loadData
  print (M.size (M.filterWithKey (\k _ -> contains dat "shiny gold" k) dat) - 1)

part2 :: IO ()
part2 = do
  dat <- loadData
  print $ containCount dat "shiny gold"
