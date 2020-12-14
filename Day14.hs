{-# LANGUAGE TemplateHaskell #-}

module Day14 where

import Control.Lens
import Data.Bits
import qualified Data.Map as M
import qualified Data.Set as S

data Instr = Mask (Integer, Integer) | Mem Integer Integer deriving (Show)

data State = State {_mask :: (Integer, Integer), _memory :: M.Map Integer Integer} deriving (Show)

makeLenses ''State

masked :: (Integer, Integer) -> Integer -> Integer
masked (z, o) i = (i .&. z) .|. o

runInstr1 :: Instr -> State -> State
runInstr1 (Mask i) s = s & mask .~ i
runInstr1 (Mem a i) s = s & memory %~ M.insert a (masked (s ^. mask) i)

runInstr2 :: Instr -> State -> State
runInstr2 (Mask i) s = s & mask .~ i
runInstr2 (Mem a i) s = s & memory %~ (\m -> foldr (`M.insert` i) m (instantiate a (s ^. mask)))

uniquePrefix :: (Eq a, Ord a) => [a] -> [a]
uniquePrefix = uniquePrefix' S.empty
  where
    uniquePrefix' s (x : xs)
      | x `elem` s = []
      | otherwise = x : uniquePrefix' (S.insert x s) xs

instantiate :: Integer -> (Integer, Integer) -> [Integer]
instantiate a (z, o) =
  let mask = complement z .|. o
      ws = uniquePrefix $ map (.&. complement mask) $ iterate (\x -> (x + 1) .|. mask) mask
   in map (\w -> (mask .&. a) .|. o .|. w) ws

repX :: Char -> String -> String
repX _ [] = []
repX c ('X' : xs) = c : repX c xs
repX c (x : xs) = x : repX c xs

readBin :: String -> Integer
readBin = readBin' 0
  where
    readBin' r [] = r
    readBin' r ('0' : xs) = readBin' (2 * r) xs
    readBin' r ('1' : xs) = readBin' (2 * r + 1) xs

parseInstr :: String -> Instr
parseInstr s
  | take 4 s == "mask" =
    let s' = drop 7 s
     in Mask (readBin (repX '1' s'), readBin (repX '0' s'))
  | otherwise =
    let s' = drop 4 s
        (adr, r) = head (reads s')
        r' = drop 4 r
     in Mem adr (read r')

loadData :: IO [Instr]
loadData = do
  dat <- readFile "Day14.dat"
  return $ map parseInstr $ lines dat

part1 :: IO ()
part1 = do
  dat <- loadData
  print $ M.foldr (+) 0 $ _memory $ foldl (flip runInstr1) (State (0, 0) M.empty) dat

part2 :: IO ()
part2 = do
  dat <- loadData
  print $ M.foldr (+) 0 $ _memory $ foldl (flip runInstr2) (State (0, 0) M.empty) dat

test = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0"