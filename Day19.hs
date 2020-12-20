{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day19 where

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Text.LParse.Parser
import Text.LParse.Prebuilt

data Rule = Term Int Char | NTerm Int [[Rule]]

idx :: Rule -> Int
idx (Term i _) = i
idx (NTerm i _) = i

parseTermBody :: Int -> Parser r String Rule
parseTermBody i = Term i <$> surround "\"\"" tokenReturn

parseNTermBody :: Int -> M.Map Int Rule -> Parser r String Rule
parseNTermBody i m = NTerm i <$> sepSome (consume " | ") (sepSome (consumeSingle ' ') ((m M.!) . fromInteger <$> integer))

parseRule :: M.Map Int Rule -> Parser r String (Int, Rule)
parseRule m = do
  idx <- fromInteger <$> integer
  consume ": "
  (idx,) <$> (parseTermBody idx <|> parseNTermBody idx m)

parseRuleset :: Parser r String (M.Map Int Rule)
parseRuleset =
  pfix
    ( \m -> do
        list <- sepMany (consumeSingle '\n') (parseRule m)
        return $ M.fromList list
    )

parseAll :: Parser r String (Rule, M.Map Int Rule, [String])
parseAll = do
  r <- parseRuleset
  consume "\n\n"
  ss <- sepMany (consumeSingle '\n') word
  return (r M.! 0, r, ss)

loadData1 :: IO (Rule, [String])
loadData1 = (\(r, _, ss) -> (r, ss)) . forceParse parseAll <$> readFile "Day19.dat"

loadData2 :: IO (Rule, M.Map Int Rule, [String])
loadData2 = forceParse parseAll <$> readFile "Day19-2.dat"

matches :: String -> [Rule] -> Maybe String
matches = foldM match

match :: String -> Rule -> Maybe String
match [] r = Nothing
match (s : ss) (Term _ t)
  | s == t = Just ss
  | otherwise = Nothing
match ss (NTerm _ n) = foldr1 (<|>) (map (matches ss) n)

part1 :: IO ()
part1 = do
  (r, ss) <- loadData1
  print $ length $ filter (\s -> match s r == Just "") ss

produces :: Rule -> (Int, Int) -> Bool
produces (Term _ _) _ = False
produces (NTerm _ rs) (a, b) = [a, b] `elem` map (map idx) rs

cykSMerge :: M.Map (Int, Int) [Int] -> (Int, Int) -> S.Set Int
cykSMerge rs p = S.fromList $ M.findWithDefault [] p rs

cykMerge :: M.Map (Int, Int) [Int] -> S.Set Int -> S.Set Int -> S.Set Int
cykMerge rs a b = foldr S.union S.empty $ S.map (cykSMerge rs) $ S.cartesianProduct a b

cykStep :: M.Map (Int, Int) [Int] -> [[S.Set Int]] -> [[S.Set Int]]
cykStep rs is = [S.unions [cykMerge rs ((is !! (length is - 1 - y)) !! x) ((is !! y) !! (x + y + 1)) | y <- [0 .. length is -1]] | x <- [0 .. length (head is) - 2]] : is

invert :: Rule -> M.Map (Int, Int) [Int]
invert (Term _ _) = M.empty
invert (NTerm i rs) = M.fromList $ map (\[a, b] -> ((idx a, idx b), [i])) rs

cykMap :: M.Map Int Rule -> M.Map (Int, Int) [Int]
cykMap = M.foldr (\r m' -> M.unionWith (++) m' (invert r)) M.empty

cyk :: M.Map Int Rule -> String -> Bool
cyk rs s =
  let ts = map (\case 'a' -> 36; 'b' -> 24) s
      cykIter ([xs] : _) = S.member 0 xs
      cykIter xss = cykIter (cykStep (cykMap rs) xss)
   in cykIter [map S.singleton ts]

part2 :: IO ()
part2 = do
  (r, rs, ss) <- loadData2
  print $ length $ filter (cyk rs) ss

test2 =
  [ "0: 1 2 | 2 1",
    "1: 36 24 | 24 36",
    "2: 24 24 | 36 36",
    "24: \"b\"",
    "36: \"a\"",
    "",
    "ab"
  ]

test =
  [ "42: 9 14 | 10 1",
    "9: 14 27 | 1 26",
    "10: 23 14 | 28 1",
    "1: \"a\"",
    "11: 42 31 | 42 11 31",
    "5: 1 14 | 15 1",
    "19: 14 1 | 14 14",
    "12: 24 14 | 19 1",
    "16: 15 1 | 14 14",
    "31: 14 17 | 1 13",
    "6: 14 14 | 1 14",
    "2: 1 24 | 14 4",
    "0: 8 11",
    "13: 14 3 | 1 12",
    "15: 1 | 14",
    "17: 14 2 | 1 7",
    "23: 25 1 | 22 14",
    "28: 16 1",
    "4: 1 1",
    "20: 14 14 | 1 15",
    "3: 5 14 | 16 1",
    "27: 1 6 | 14 18",
    "14: \"b\"",
    "21: 14 1 | 1 14",
    "25: 1 1 | 1 14",
    "22: 14 14",
    "8: 42 | 42 8",
    "26: 14 22 | 1 20",
    "18: 15 15",
    "7: 14 5 | 1 21",
    "24: 14 1",
    "",
    "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
    "bbabbbbaabaabba",
    "babbbbaabbbbbabbbbbbaabaaabaaa",
    "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
    "bbbbbbbaaaabbbbaaabbabaaa",
    "bbbababbbbaaaaaaaabbababaaababaabab",
    "ababaaaaaabaaab",
    "ababaaaaabbbaba",
    "baabbaaaabbaaaababbaababb",
    "abbbbabbbbaaaababbbbbbaaaababb",
    "aaaaabbaabaaaaababaa",
    "aaaabbaaaabbaaa",
    "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
    "babaaabbbaaabaababbaabababaaab",
    "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
  ]
