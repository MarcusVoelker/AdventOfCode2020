{-# LANGUAGE TupleSections #-}

module Day22 where

import qualified Data.Set as S

loadData :: IO ([Int], [Int])
loadData = do
  dat <- readFile "Day22.dat"
  let p1 = takeWhile (not . null) $ tail $ lines dat
  let p2 = drop 2 $ dropWhile (not . null) $ tail $ lines dat
  return (map read p1, map read p2)

score :: [Int] -> Int
score = sum . zipWith (*) [1 ..] . reverse

step :: ([Int], [Int]) -> Either Int ([Int], [Int])
step ([], x) = Left $ score x
step (x, []) = Left $ score x
step (x : xs, y : ys)
  | x > y = Right (xs ++ [x, y], ys)
  | otherwise = Right (xs, ys ++ [y, x])

type State = ([Int], [Int])

recStep :: (State, S.Set State) -> Either (Int, Bool) (State, S.Set State)
recStep (([], x), _) = Left (score x, False)
recStep ((x, []), _) = Left (score x, True)
recStep ((x : xs, y : ys), s)
  | (x : xs, y : ys) `S.member` s = Left (score (x : xs), True)
  | x <= length xs && y <= length ys =
    let (_, w) = recComb (take x xs, take y ys)
     in if w then Right ((xs ++ [x, y], ys), S.insert (x : xs, y : ys) s) else Right ((xs, ys ++ [y, x]), S.insert (x : xs, y : ys) s)
  | x > y = Right ((xs ++ [x, y], ys), S.insert (x : xs, y : ys) s)
  | otherwise = Right ((xs, ys ++ [y, x]), S.insert (x : xs, y : ys) s)

recComb :: ([Int], [Int]) -> (Int, Bool)
recComb = eitherate recStep . (,S.empty)

eitherate :: (a -> Either b a) -> a -> b
eitherate f x = let a = f x in case a of (Left b) -> b; (Right a') -> eitherate f a'

part1 :: IO ()
part1 = do
  dat <- loadData
  print $ eitherate step dat

part2 :: IO ()
part2 = do
  dat <- loadData
  print $ recComb dat