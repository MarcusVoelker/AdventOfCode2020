{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.IntMap.Strict as M
import Data.List

loadData :: IO [Int]
loadData = do
  dat <- readFile "Day15.dat"
  return $ read $ '[' : dat ++ "]"

step :: (Int, Int, M.IntMap Int) -> (Int, Int, M.IntMap Int)
step (!i, !l, !m)
  | l `M.member` m =
    let l' = i - m M.! l - 1
        m' = M.insert l (i -1) m
     in (i + 1, l', m')
  | otherwise =
    let m' = M.insert l (i -1) m
     in (i + 1, 0, m')

initial :: [Int] -> (Int, Int, M.IntMap Int)
initial xs = (length xs, last xs, M.fromList occs)
  where
    occs = zip (init xs) [0 ..]

generate :: [Int] -> [Int]
generate xs = init xs ++ map (\(_, x, _) -> x) (iterate step (initial xs))

part1 :: IO ()
part1 = do
  dat <- loadData
  print (generate dat !! 2019)

part2 :: IO ()
part2 = do
  dat <- loadData
  print (generate dat !! 29999999)

main :: IO ()
main = part2