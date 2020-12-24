module Day23 where

import Data.List
import Data.Maybe
import Data.CircularList

loadData :: IO (CList Int)
loadData = do
    dat <- readFile "Day23.dat"
    return $ fromList $ map (read . return) dat

step :: CList Int -> CList Int
step xs = let 
    cp = fromJust $ focus xs
    three = take 3 $ tail $ rightElements xs
    rest = removeR $ removeR $ removeR $ rotR xs
    np = head $ dropWhile (`notElem` toList rest) $ tail $ iterate (\x -> mod (x-1) (length xs + 2)) cp
    rt = fromJust $ rotateTo np rest 
    rt' = insertL (three !! 2) $ insertL (three!! 1) $ insertL (head three) rt
    in
        rotR $ fromJust $ rotateTo cp rt'

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ = id
nTimes i f = nTimes (i-1) f . f

part1 :: IO ()
part1 = do
    dat <- loadData
    print $ nTimes 100 step dat

part2 :: IO ()
part2 = do
    dat <- loadData
    print $ nTimes 100 step $ fromList (toList dat ++ [10..1000000])