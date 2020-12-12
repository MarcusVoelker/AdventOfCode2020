module Day12 where

data Dir = DU | DL | DD | DR deriving (Ord,Eq,Show)

type State1 = (Int,Int,Dir)
type State2 = ((Int,Int),(Int,Int))
data Instr = N Int | S Int | E Int | W Int | L Int | R Int | F Int

parseInstr :: String -> Instr
parseInstr ('N':x) = N $ read x
parseInstr ('S':x) = S $ read x
parseInstr ('E':x) = E $ read x
parseInstr ('W':x) = W $ read x
parseInstr ('L':x) = L $ read x
parseInstr ('R':x) = R $ read x
parseInstr ('F':x) = F $ read x

loadData :: IO [Instr]
loadData = do
    dat <- readFile "Day12.dat"
    return $ map parseInstr $ lines dat

turn1 :: Int -> Dir -> Dir
turn1 0 d = d 
turn1 i DU
    | i > 0 = turn1 (i-90) DL 
    | i < 0 = turn1 (i+90) DR 
turn1 i DL
    | i > 0 = turn1 (i-90) DD 
    | i < 0 = turn1 (i+90) DU 
turn1 i DD
    | i > 0 = turn1 (i-90) DR 
    | i < 0 = turn1 (i+90) DL 
turn1 i DR
    | i > 0 = turn1 (i-90) DU 
    | i < 0 = turn1 (i+90) DD 

step1 :: Instr -> State1 -> State1
step1 (N i) (x,y,d) = (x,y+i,d)
step1 (S i) (x,y,d) = (x,y-i,d)
step1 (E i) (x,y,d) = (x+i,y,d)
step1 (W i) (x,y,d) = (x-i,y,d)
step1 (L i) (x,y,d) = (x,y,turn1 i d)
step1 (R i) (x,y,d) = (x,y,turn1 (-i) d)
step1 (F i) (x,y,DU) = (x,y+i,DU)
step1 (F i) (x,y,DL) = (x-i,y,DL)
step1 (F i) (x,y,DD) = (x,y-i,DD)
step1 (F i) (x,y,DR) = (x+i,y,DR)

turn2 :: Int -> (Int,Int) -> (Int,Int)
turn2 0 w = w
turn2 i (x,y)
    | i > 0 = turn2 (i-90) (-y,x)
    | i < 0 = turn2 (i+90) (y,-x)

step2 :: Instr -> State2 -> State2
step2 (N i) (s,(wx,wy)) = (s,(wx,wy+i))
step2 (S i) (s,(wx,wy)) = (s,(wx,wy-i))
step2 (E i) (s,(wx,wy)) = (s,(wx+i,wy))
step2 (W i) (s,(wx,wy)) = (s,(wx-i,wy))
step2 (L i) (s,w) = (s,turn2 i w)
step2 (R i) (s,w) = (s,turn2 (-i) w)
step2 (F i) ((x,y),(wx,wy)) = ((x+i*wx,y+i*wy),(wx,wy))

part1 :: IO ()
part1 = do
    dat <- loadData
    print $ (\(x,y,_) -> abs x+ abs y) $ foldl (flip step1) (0,0,DR) dat

part2 :: IO ()
part2 = do
    dat <- loadData
    print $ (\((x,y),_) -> abs x+ abs y) $ foldl (flip step2) ((0,0),(10,1)) dat