module Day8 where

import Control.Lens
import Data.Maybe (isNothing)
import qualified Data.Set as S
import Interpreter

loadData :: IO Program
loadData = do
  dat <- readFile "Day8.dat"
  return $ parseProgram dat

mutate :: Instruction -> Instruction
mutate (Jmp x) = Nop x
mutate (Nop x) = Jmp x

mutations :: Program -> [Program]
mutations [] = [[]]
mutations (Acc n : is) = map (Acc n :) (mutations is)
mutations (i : is) = (mutate i : is) : map (i :) (mutations is)

findLoop :: [PState] -> Maybe Int
findLoop = findLoop' S.empty
  where
    findLoop' _ [] = Nothing
    findLoop' s (p : ps)
      | S.member (pc p) s = Just $ p ^. acc
      | otherwise = findLoop' (S.insert (pc p) s) ps

part1 :: IO ()
part1 = do
  dat <- loadData
  let t = traceProgram dat
  print $ findLoop t

part2 :: IO ()
part2 = do
  dat <- loadData
  print $ _acc $ last $ head $ filter (isNothing . findLoop) $ map traceProgram $ mutations dat
