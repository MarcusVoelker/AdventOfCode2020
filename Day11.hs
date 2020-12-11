module Day11 where

data Cell = Empty | Seat | Occ deriving Eq

instance Show Cell where
  show Empty = "."
  show Seat = "L"
  show Occ = "#"

instance Read Cell where
  readsPrec _ s = case head s of
    '.' -> [(Empty, tail s)]
    'L' -> [(Seat, tail s)]
    '#' -> [(Occ, tail s)]
    _ -> []

loadData :: IO [[Cell]]
loadData = do
  dat <- readFile "Day11.dat"
  return $ map (map (read . return)) $ lines dat

localStep1 :: ((Cell,Cell,Cell),(Cell,Cell,Cell),(Cell,Cell,Cell)) -> Cell
localStep1 ((_,_,_),(_,Empty,_),(_,_,_)) = Empty
localStep1 ((a,b,c),(d,Seat,f),(g,h,i)) = if Occ `elem` [a,b,c,d,f,g,h,i] then Seat else Occ
localStep1 ((a,b,c),(d,Occ,f),(g,h,i)) = if length (filter (==Occ) [a,b,c,d,f,g,h,i]) >= 4 then Seat else Occ

localStep2 :: ((Cell,Cell,Cell),(Cell,Cell,Cell),(Cell,Cell,Cell)) -> Cell
localStep2 ((_,_,_),(_,Empty,_),(_,_,_)) = Empty
localStep2 ((a,b,c),(d,Seat,f),(g,h,i)) = if Occ `elem` [a,b,c,d,f,g,h,i] then Seat else Occ
localStep2 ((a,b,c),(d,Occ,f),(g,h,i)) = if length (filter (==Occ) [a,b,c,d,f,g,h,i]) >= 5 then Seat else Occ

step1 :: [[Cell]] -> [[Cell]]
step1 cs = let 
  zr = map (\r -> zip3 (Empty:r) r (tail r++[Empty])) cs
  z = zipWith3 zip3 (repeat (Empty,Empty,Empty):zr) zr (tail zr ++ [repeat (Empty,Empty,Empty)]) in
    map (map localStep1) z

dirN :: [[Cell]] -> (Int,Int) -> (Int,Int) -> Cell
dirN cs (x,y) (dx,dy) 
  | x+dx < 0 || y+dy < 0 || x+dx >= length (head cs) || y+dy >= length cs = Empty
  | (cs!!(y+dy))!!(x+dx) /= Empty = (cs!!(y+dy))!!(x+dx)
  | otherwise = dirN cs (x+dx,y+dy) (dx,dy) 

rayNeigh :: [[Cell]] -> (Int,Int) -> ((Cell,Cell,Cell),(Cell,Cell,Cell),(Cell,Cell,Cell))
rayNeigh cs (x,y) = (
  (dirN cs (x,y) (-1,-1),dirN cs (x,y) (0,-1),dirN cs (x,y) (1,-1)),
  (dirN cs (x,y) (-1,0) ,(cs!!y)!!x          ,dirN cs (x,y) (1,0)),
  (dirN cs (x,y) (-1,1) ,dirN cs (x,y) (0,1) ,dirN cs (x,y) (1,1)))

step2 :: [[Cell]] -> [[Cell]]
step2 cs = let 
  ann = [[(x,y) | x <- [0..length (head cs) -1]] | y <- [0..length cs -1]]
  z = map (map (\(x,y) -> rayNeigh cs (x,y))) ann in
    map (map localStep2) z


fixit :: (Eq a) => (a -> a) -> a -> a
fixit f x | f x == x = x | otherwise = fixit f (f x) 

part1 :: IO ()
part1 = do
  dat <- loadData
  print $ sum $ map (length . filter (==Occ)) $ fixit step1 dat

part2 :: IO ()
part2 = do
  dat <- loadData
  print $ sum $ map (length . filter (==Occ)) $ fixit step2 dat