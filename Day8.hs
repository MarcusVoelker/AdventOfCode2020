module Day8 where

import Data.List (unfoldr)
import Data.Maybe (isNothing)
import qualified Data.Set as S

data Instruction = Acc Int | Jmp Int | Nop Int deriving (Eq, Show)

data PState = PState {after :: [Instruction], before :: [Instruction], acc :: Int} deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction s
  | take 3 s == "acc" = Acc $ read $ drop (if s !! 4 == '+' then 5 else 4) s
  | take 3 s == "jmp" = Jmp $ read $ drop (if s !! 4 == '+' then 5 else 4) s
  | take 3 s == "nop" = Nop $ read $ drop (if s !! 4 == '+' then 5 else 4) s

loadData :: IO [Instruction]
loadData = do
  dat <- readFile "Day8.dat"
  return $ map parseInstruction $ lines dat

initial :: [Instruction] -> PState
initial is = PState is [] 0

stepProgram :: PState -> Maybe PState
stepProgram p
  | null $ after p = Nothing
  | otherwise =
    let i = head $ after p
     in Just $ case i of
          Acc x -> PState (tail $ after p) (i : before p) (acc p + x)
          Jmp x
            | x < 0 -> PState (reverse (take (- x) $ before p) ++ after p) (drop (- x) $ before p) (acc p)
            | x >= 0 -> PState (drop x $ after p) (reverse (take x $ after p) ++ before p) (acc p)
          Nop _ -> PState (tail $ after p) (i : before p) (acc p)

traceProgram :: [Instruction] -> [PState]
traceProgram = (\x -> x : unfoldr (fmap (\s -> (s, s)) . stepProgram) x) . initial

pc :: PState -> Int
pc = length . before

mutate :: Instruction -> Instruction
mutate (Jmp x) = Nop x
mutate (Nop x) = Jmp x

mutations :: [Instruction] -> [[Instruction]]
mutations [] = [[]]
mutations (Acc n : is) = map (Acc n :) (mutations is)
mutations (i : is) = (mutate i : is) : map (i :) (mutations is)

findLoop :: [PState] -> Maybe Int
findLoop = findLoop' S.empty
  where
    findLoop' _ [] = Nothing
    findLoop' s (p : ps)
      | S.member (pc p) s = Just $ acc p
      | otherwise = findLoop' (S.insert (pc p) s) ps

part1 :: IO ()
part1 = do
  dat <- loadData
  let t = traceProgram dat
  print $ findLoop t

part2 :: IO ()
part2 = do
  dat <- loadData
  print $ acc $ last $ head $ filter (isNothing . findLoop) $ map traceProgram $ mutations dat
