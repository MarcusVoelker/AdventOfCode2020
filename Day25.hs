{-# LANGUAGE BangPatterns #-}
module Day25 where

loadData :: IO (Integer,Integer)
loadData = do
    dat <- readFile "Day25.dat"
    return $ (\[x,y] -> (read x, read y)) $ lines dat

subject :: Integer
subject = 7

modulus :: Integer
modulus = 20201227

guessLoop :: Integer -> Integer
guessLoop = guessLoop' 1 0
    where
        guessLoop' v !s g | v == g = s
            | otherwise = guessLoop' (mod (v*subject) modulus) (s+1) g

powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 _ = 1
powMod b e m = let r = powMod (mod (b*b) m) (div e 2) m in
    if mod e 2 == 1 then mod (r*b) m else r

part1 :: IO ()
part1 = do
    (c,d) <- loadData
    print $ powMod 7 (guessLoop c*guessLoop d) modulus