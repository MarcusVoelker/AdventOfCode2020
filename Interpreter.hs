{-# LANGUAGE TemplateHaskell #-}

module Interpreter where

import Control.Lens
import Data.List (unfoldr)

data Instruction = Acc Int | Jmp Int | Nop Int deriving (Eq, Show)

type Program = [Instruction]

data PState = PState {_after :: Program, _before :: Program, _acc :: Int} deriving (Show)

makeLenses ''PState

parseInstruction :: String -> Instruction
parseInstruction s
  | take 3 s == "acc" = Acc $ read $ drop (if s !! 4 == '+' then 5 else 4) s
  | take 3 s == "jmp" = Jmp $ read $ drop (if s !! 4 == '+' then 5 else 4) s
  | take 3 s == "nop" = Nop $ read $ drop (if s !! 4 == '+' then 5 else 4) s

parseProgram :: String -> Program
parseProgram = map parseInstruction . lines

initial :: Program -> PState
initial is = PState is [] 0

stepProgram :: PState -> Maybe PState
stepProgram p
  | null $ p ^. after = Nothing
  | otherwise =
    let i = head $ p ^. after
     in Just $ case i of
          Acc x -> ((p & after %~ tail) & before %~ (i :)) & acc +~ x
          Jmp x
            | x < 0 -> (p & after %~ (reverse (take (- x) $ p ^. before) ++)) & before %~ drop (- x)
            | x >= 0 -> (p & after %~ drop x) & before %~ (reverse (take x $ p ^. after) ++)
          Nop _ -> (p & after %~ tail) & before %~ (i :)

traceProgram :: Program -> [PState]
traceProgram = (\x -> x : unfoldr (fmap (\s -> (s, s)) . stepProgram) x) . initial

pc :: PState -> Int
pc = length . _before