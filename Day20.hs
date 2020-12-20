module Day20 where

import Data.List
import qualified Data.Map as M
import Data.Maybe

data Tile = Tile {idt :: Int, raw :: [String], nT :: Maybe Int, nR :: Maybe Int, nB :: Maybe Int, nL :: Maybe Int} deriving (Show, Eq, Ord)

rotateL :: Tile -> Tile
rotateL (Tile i s t r b l) = Tile i (reverse $ transpose s) r b l t

rotateR :: Tile -> Tile
rotateR (Tile i s t r b l) = Tile i (map reverse $ transpose s) l t r b

rotate180 :: Tile -> Tile
rotate180 (Tile i s t r b l) = Tile i (map reverse $ reverse s) b l t r

mirrorN :: Tile -> Tile
mirrorN (Tile i s t r b l) = Tile i (transpose s) l b r t

mirrorH :: Tile -> Tile
mirrorH (Tile i s t r b l) = Tile i (map reverse s) t l b r

mirrorV :: Tile -> Tile
mirrorV (Tile i s t r b l) = Tile i (reverse s) b r t l

mirrorP :: Tile -> Tile
mirrorP (Tile i s t r b l) = Tile i (reverse s) r t l b

switchToFit :: [Maybe (Maybe Int)] -> Tile -> Tile
switchToFit spec t =
  let arr = [nT t, nR t, nB t, nL t]
      borders = catMaybes $ zipWith (\s b -> if isNothing s then Nothing else Just (fromJust s == b)) spec arr
      defborders = catMaybes $ zipWith (\s b -> if isNothing s || isNothing (fromJust s) then Nothing else Just (fromJust s == b)) spec arr
   in if and borders then t else if or defborders then switchToFit spec (mirrorN t) else switchToFit spec (rotateR t)

borders :: Tile -> [String]
borders t =
  let r = raw t
   in [head r, map last r, last r, map head r]

bCount :: Tile -> Int
bCount t = length $ catMaybes [nT t, nR t, nB t, nL t]

parseTile :: [String] -> Tile
parseTile (t : cd) = Tile (read (take 4 (drop 5 t))) cd Nothing Nothing Nothing Nothing

parseTiles :: [String] -> [Tile]
parseTiles [] = []
parseTiles xs = parseTile (take 11 xs) : parseTiles (drop 12 xs)

loadData :: IO [Tile]
loadData = do
  dat <- readFile "Day20.dat"
  return $ parseTiles $ lines dat

match :: Tile -> Tile -> Bool
match t1 t2 = not (null (borders t1 `intersect` borders t2) && null (borders t1 `intersect` map reverse (borders t2)))

matches :: Tile -> [Tile] -> [Maybe Int]
matches t ts = map ((idt <$>) . (\b -> find (\t' -> b `elem` borders t' || reverse b `elem` borders t') (ts \\ [t]))) $ borders t

possNeighs :: [Tile] -> M.Map Tile [Tile]
possNeighs ts = M.fromList $ map (\t -> possNeighs' t (ts \\ [t])) ts
  where
    possNeighs' t ts = (t, filter (match t) ts)

topLeftCorner :: Int
topLeftCorner = 1699

topLine :: M.Map Int Tile -> M.Map Int Tile
topLine ts =
  let ts' = M.adjust (switchToFit [Nothing, Just (Just 3529), Just (Just 2251), Nothing]) 1699 ts
      stepMap t ts = M.adjust (switchToFit [Just Nothing, Nothing, Nothing, Just (Just $ idt t)]) (fromJust $ nR t) ts
      step (t, ts) = (stepMap t ts M.! fromJust (nR t), stepMap t ts)
      run (t, ts)
        | bCount (ts M.! fromJust (nR t)) == 2 = ts
        | otherwise = run $ step (t, ts)
   in run (ts' M.! 1699, ts')

tailLine :: (Int, M.Map Int Tile) -> (Int, M.Map Int Tile)
tailLine (i, ts) =
  let t0 = fromJust (nB (ts M.! i))
      c0 = bCount (ts M.! t0)
      ts' = M.adjust (switchToFit [Just (Just i), Nothing, Nothing, Just Nothing]) t0 ts
      stepMap i t ts = M.adjust (switchToFit [Just (Just i), Nothing, Nothing, Just (Just $ idt t)]) (fromJust $ nR t) ts
      step (i, t, ts) = (fromJust (nR (ts' M.! i)), stepMap i t ts M.! fromJust (nR t), stepMap i t ts)
      run (i, t, ts)
        | bCount (ts M.! fromJust (nR t)) == c0 = ts
        | otherwise = run $ step (i, t, ts)
   in (t0, run (i, ts' M.! t0, ts'))

sortAll :: M.Map Int Tile -> M.Map Int Tile
sortAll ts =
  let ts' = topLine ts
      run (t, ts)
        | bCount (ts M.! t) == 2 = ts
        | otherwise = run $ tailLine (t, ts)
   in run $ tailLine (1699, ts')

extractLine :: Int -> M.Map Int Tile -> [Tile]
extractLine i ts
  | isNothing $ nR (ts M.! i) = [ts M.! i]
  | otherwise = ts M.! i : extractLine (fromJust $ nR (ts M.! i)) ts

extractGrid :: M.Map Int Tile -> [[Tile]]
extractGrid m =
  let run i
        | isNothing $ nB (m M.! i) = [extractLine i m]
        | otherwise = extractLine i m : run (fromJust $ nB (m M.! i))
   in run 1699

fillNeighs :: [Tile] -> M.Map Int Tile
fillNeighs ts = M.fromList $ map (\t -> let [a, b, c, d] = matches t ts in (idt t, Tile (idt t) (raw t) a b c d)) ts

part1 :: IO ()
part1 = do
  dat <- loadData
  print $ product $ map idt $ M.keys $ M.filter ((== 2) . length) $ possNeighs dat