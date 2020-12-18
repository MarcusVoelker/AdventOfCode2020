{-# LANGUAGE TupleSections #-}

module Day18 where

import Control.Applicative
import Text.LParse.Parser
import Text.LParse.Prebuilt

data Expr = Con Int | Add Expr Expr | Mul Expr Expr deriving (Show)

parseSingle1 :: Parser r String Expr
parseSingle1 =
  surround "()" parseExpr1
    <|> (Con . fromInteger <$> integer)

parseSingle2 :: Parser r String Expr
parseSingle2 =
  surround "()" parseExpr2
    <|> (Con . fromInteger <$> integer)

parseOp :: Parser r String Expr
parseOp = do
  p1 <- parseSingle1
  r <- some ((('+',) <$> (consumeSingle '+' >> parseSingle1)) <|> (('*',) <$> (consumeSingle '*' >> parseSingle1)))
  return $ foldl (\e1 (op, e2) -> case op of '+' -> Add e1 e2; '*' -> Mul e1 e2) p1 r

parseAdd :: Parser r String Expr
parseAdd = do
  p1 <- parseSingle2
  r <- many (consumeSingle '+' >> parseSingle2)
  return $ foldl Add p1 r

parseMul :: Parser r String Expr
parseMul = do
  p1 <- parseAdd
  r <- many (consumeSingle '*' >> parseAdd)
  return $ foldl Mul p1 r

parseExpr1 :: Parser r String Expr
parseExpr1 = parseOp <|> parseSingle1

parseExpr2 :: Parser r String Expr
parseExpr2 = parseMul <|> parseSingle2

loadData1 :: IO [Expr]
loadData1 = do
  dat <- readFile "Day18.dat"
  return $ map (forceParse (skipWhitespace parseExpr1)) $ lines dat

loadData2 :: IO [Expr]
loadData2 = do
  dat <- readFile "Day18.dat"
  return $ map (forceParse (skipWhitespace parseExpr2)) $ lines dat

eval :: Expr -> Int
eval (Con i) = i
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

part1 :: IO ()
part1 = do
  dat <- loadData1
  print $ sum $ map eval dat

part2 :: IO ()
part2 = do
  dat <- loadData2
  print $ sum $ map eval dat
