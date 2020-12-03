module Day3 where

loadData :: IO [[Bool]]
loadData = do
    dat <- readFile "Day3.dat"
    return $ map (cycle . map (=='#')) $ lines dat

slopeCheck :: Int -> Int -> [[Bool]] -> Int
slopeCheck x y = length . filter (\(i,r) -> r!!(div i y*x) && mod i y == 0) . zip [0..]

part1 :: IO ()
part1 = loadData >>= (print . slopeCheck 3 1)

part2 :: IO ()
part2 = do
    dat <- loadData
    print $ product $ map (\(x,y) -> slopeCheck x y dat) [(1,1),(3,1),(5,1),(7,1),(1,2)]