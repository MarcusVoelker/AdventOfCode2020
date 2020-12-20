module Day20 where

import Data.List
import qualified Data.Map as M

data Tile = Tile {idt :: Int, raw :: [String]} deriving (Show, Eq, Ord)

borders :: Tile -> [String]
borders t =
  let r = raw t
   in [head r, map head r, last r, map last r]

parseTile :: [String] -> Tile
parseTile (t : cd) = Tile (read (take 4 (drop 5 t))) cd

parseTiles :: [String] -> [Tile]
parseTiles [] = []
parseTiles xs = parseTile (take 11 xs) : parseTiles (drop 12 xs)

loadData :: IO [Tile]
loadData = do
  dat <- readFile "Day20.dat"
  return $ parseTiles $ lines dat

match :: Tile -> Tile -> Bool
match t1 t2 = not (null (borders t1 `intersect` borders t2) && null (borders t1 `intersect` map reverse (borders t2)))

possNeighs :: [Tile] -> M.Map Tile [Tile]
possNeighs ts = M.fromList $ map (\t -> possNeighs' t (ts \\ [t])) ts
  where
    possNeighs' t ts = (t, filter (match t) ts)

part1 :: IO ()
part1 = do
  dat <- loadData
  print $ product $ map idt $ M.keys $ M.filter ((== 2) . length) $ possNeighs dat