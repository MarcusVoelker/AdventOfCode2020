module Day24 where

import Data.List
import qualified Data.Set as S

data Dir = W | NW | NE | E | SE | SW deriving Show

readDirs :: String -> [Dir]
readDirs [] = []
readDirs ('n':'e':xs) = NE : readDirs xs
readDirs ('n':'w':xs) = NW : readDirs xs
readDirs ('s':'e':xs) = SE : readDirs xs
readDirs ('s':'w':xs) = SW : readDirs xs
readDirs ('e':xs) = E : readDirs xs
readDirs ('w':xs) = W : readDirs xs

loadData :: IO [[Dir]]
loadData = do
    dat <- readFile "Day24.dat"
    return $ map readDirs $ lines dat

step :: Dir -> (Int,Int) -> (Int,Int)
step W (x,y)  = (x-1,y)
step NW (x,y) = (x-1,y+1)
step NE (x,y) = (x,y+1)
step E (x,y)  = (x+1,y)
step SE (x,y) = (x+1,y-1)
step SW (x,y) = (x,y-1)

flipTile :: [Dir] -> S.Set (Int,Int) -> S.Set(Int,Int)
flipTile ds s = let t = foldr step (0,0) ds in
    if S.member t s then S.delete t s else S.insert t s

initial :: [[Dir]] -> S.Set (Int,Int)
initial = foldr flipTile S.empty

gridize :: S.Set (Int,Int) -> [[Bool]]
gridize s = let
    xmin = minimum $ S.map fst s
    xmax = maximum $ S.map fst s
    ymin = minimum $ S.map snd s
    ymax = maximum $ S.map snd s
    in [[S.member (x,y) s | x <- [xmin..xmax]] | y <- [ymin..ymax]]

count :: Bool -> Int 
count b = sum [ 1 | b ]

neighs :: [[Bool]] -> [[(Int,Bool)]]
neighs grid = let
    z1 = map (\s -> zip3 (False:s) s (tail s++[False])) grid
    z2 = zipWith3 (zipWith3 (\(aa,ab,ac)(ba,bb,bc)(ca,cb,cc) -> (sum (map count [ac,ab,ba,bc,cb,ca]),bb))) (repeat (False,False,False):z1) z1 (tail z1 ++ [repeat (False,False,False)])
    in z2

neighStep :: (Int,Bool) -> Bool
neighStep (1,True) = True
neighStep (2,True) = True
neighStep (2,False) = True
neighStep _ = False

extend :: [[Bool]] -> [[Bool]]
extend = (\x -> let l = False <$[1..length (head x)] in l:x++[l]) . map ((++[False]) . (False:))

gridStep :: [[Bool]] -> [[Bool]] 
gridStep = map (map neighStep) . neighs . extend

part1 :: IO ()
part1 = do
    dat <- loadData
    print $ length $ initial dat

part2 :: IO ()
part2 = do
    dat <- loadData
    print $ length $ filter id $ concat $ iterate gridStep (gridize $ initial dat)!!100

test = map readDirs [
    "sesenwnenenewseeswwswswwnenewsewsw",
    "neeenesenwnwwswnenewnwwsewnenwseswesw",
    "seswneswswsenwwnwse",
    "nwnwneseeswswnenewneswwnewseswneseene",
    "swweswneswnenwsewnwneneseenw",
    "eesenwseswswnenwswnwnwsewwnwsene",
    "sewnenenenesenwsewnenwwwse",
    "wenwwweseeeweswwwnwwe",
    "wsweesenenewnwwnwsenewsenwwsesesenwne",
    "neeswseenwwswnwswswnw",
    "nenwswwsewswnenenewsenwsenwnesesenew",
    "enewnwewneswsewnwswenweswnenwsenwsw",
    "sweneswneswneneenwnewenewwneswswnese",
    "swwesenesewenwneswnwwneseswwne",
    "enesenwswwswneneswsenwnewswseenwsese",
    "wnwnesenesenenwwnenwsewesewsesesew",
    "nenewswnwewswnenesenwnesewesw",
    "eneswnwswnwsenenwnwnwwseeswneewsenese",
    "neswnwewnwnwseenwseesewsenwsweewe",
    "wseweeenwnesenwwwswnew"
    ]