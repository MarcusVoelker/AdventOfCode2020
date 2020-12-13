{-# LANGUAGE TupleSections#-}
module Day13 where

import Data.List
import Data.Maybe
import Data.Ord

readTime :: String -> Maybe Integer
readTime "x" = Nothing
readTime i = Just $ read i

loadData :: IO (Integer,[Maybe Integer])
loadData = do
    dat <- lines <$> readFile "Day13.dat"
    return (read (dat!!0), map readTime $ words $ map (\x -> if x == ',' then ' ' else x) (dat!!1))

findEarliest :: Integer -> [Integer] -> (Integer,Integer)
findEarliest start times = minimumBy (comparing snd) $ map (\i -> (i,(div start i + 1)*i)) times

egcd :: Integer -> Integer -> (Integer,Integer)
egcd _ 0 = (1,0)
egcd a b = (t, s - q*t)
    where
        (s,t) = egcd b r
        (q,r) = a `quotRem` b

cr2 :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
cr2 (a1,n1) (a2,n2) = let (m1,m2) = egcd n1 n2 in
    (mod (a1*m2*n2+a2*m1*n1) (n1*n2),n1*n2)

matchList :: [(Integer,Integer)] -> (Integer,Integer)
matchList xs =
    let mods = map (\(n,r) -> (mod (-r) n,n)) xs in
        foldl1 cr2 mods

part1 :: IO ()
part1 = do
    (start,times) <- loadData
    print $ (\(i,t) -> (t-start)*i) $ findEarliest start (catMaybes times)

part2 :: IO ()
part2 = do
    (_,times) <- loadData
    print $ matchList $ catMaybes $ zipWith (\i -> ((,i) <$>)) [0..] times