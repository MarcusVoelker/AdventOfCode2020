module Day2 where

data Policy = Policy {minCount :: Int, maxCount :: Int, char :: Char, pass :: String} deriving Show

parse1 :: String -> (Int,String)
parse1 = head . reads

parse2 :: String -> (Int,String)
parse2 = head . reads . tail

parse3 :: String -> (Char,String)
parse3 = (\(x:xs) -> (x,xs)) . tail

parse4 :: String -> String
parse4 =  tail . tail

parseDatum :: String -> Policy
parseDatum s = 
    let (min,s') = parse1 s in
    let (max,s'') = parse2 s' in
    let (c,s''') = parse3 s'' in
    let p = parse4 s''' in
    Policy min max c p

loadData :: IO [Policy]
loadData = do
    dat <- readFile "Day2.dat"
    return $ (map parseDatum . lines) dat

correct1 :: Policy -> Bool
correct1 p = 
    let c = length (filter (==char p) (pass p)) in
    minCount p <= c && c <= maxCount p

correct2 :: Policy -> Bool
correct2 p = (pass p!!(minCount p - 1) == char p) /= (pass p!!(maxCount p - 1) == char p)

part1 :: IO ()
part1 = do
    d <- loadData
    print $ length $ filter correct1 d

part2 :: IO ()
part2 = do
    d <- loadData
    print $ length $ filter correct2 d