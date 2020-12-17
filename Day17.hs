module Day17 where

type Trip a = (a, a, a)

type Neigh3 = Trip (Trip (Trip Bool))

type Neigh4 = Trip Neigh3

ft1 :: Trip Bool
ft1 = (False, False, False)

ft2 :: Trip (Trip Bool)
ft2 = (ft1, ft1, ft1)

ft3 :: Neigh3
ft3 = (ft2, ft2, ft2)

ft4 :: Neigh4
ft4 = (ft3, ft3, ft3)

loadData :: IO [[[Bool]]]
loadData = do
  dat <- readFile "Day17.dat"
  return [map (map (== '#')) $ lines dat]

neighs3 :: [[[Bool]]] -> [[[Neigh3]]]
neighs3 s =
  let z1 = map (map (\l -> zip3 (False : l) l (tail l ++ [False]))) s
      z2 = map (\l -> zipWith3 zip3 (repeat ft1 : l) l (tail l ++ [repeat ft1])) z1
   in zipWith3 (zipWith3 zip3) (repeat (repeat ft2) : z2) z2 (tail z2 ++ [repeat (repeat ft2)])

neighs4 :: [[[[Bool]]]] -> [[[[Neigh4]]]]
neighs4 s =
  let z1 = map (map (map (\l -> zip3 (False : l) l (tail l ++ [False])))) s
      z2 = map (map (\l -> zipWith3 zip3 (repeat ft1 : l) l (tail l ++ [repeat ft1]))) z1
      z3 = map (\l -> zipWith3 (zipWith3 zip3) (repeat (repeat ft2) : l) l (tail l ++ [repeat (repeat ft2)])) z2
   in zipWith3 (zipWith3 (zipWith3 zip3)) (repeat (repeat (repeat ft3)) : z3) z3 (tail z3 ++ [repeat (repeat (repeat ft3))])

countT1 :: Trip Bool -> Int
countT1 (a, b, c) = sum [1 | True <- [a, b, c]]

countT2 :: Trip (Trip Bool) -> Int
countT2 (a, b, c) = sum (map countT1 [a, b, c])

countT3 :: Neigh3 -> Int
countT3 (a, b, c) = sum (map countT2 [a, b, c])

count3 :: Neigh3 -> (Bool, Int)
count3 (t, (l, (f, c, b), r), d) =
  let n = countT2 t + countT2 d + countT1 l + countT1 r + sum [1 | True <- [f, b]]
   in (c, n)

count4 :: Neigh4 -> (Bool, Int)
count4 (x, (t, (l, (f, c, b), r), d), y) =
  let n = countT3 x + countT3 y + countT2 t + countT2 d + countT1 l + countT1 r + sum [1 | True <- [f, b]]
   in (c, n)

check :: (Bool, Int) -> Bool
check (True, 2) = True
check (True, 3) = True
check (False, 3) = True
check _ = False

extendGrid3 :: [[[Bool]]] -> [[[Bool]]]
extendGrid3 s =
  let e1 = map (map (\r -> False : r ++ [False])) s
      e2 = map (\r -> let a = False <$ [1 .. length (head r)] in a : r ++ [a]) e1
      a = (False <$ [1 .. length (head (head e2))]) <$ [1 .. length (head e2)]
   in a : e2 ++ [a]

extendGrid4 :: [[[[Bool]]]] -> [[[[Bool]]]]
extendGrid4 s =
  let e1 = map (map (map (\r -> False : r ++ [False]))) s
      e2 = map (map (\r -> let a = False <$ [1 .. length (head r)] in a : r ++ [a])) e1
      e3 = map (\r -> let a = (False <$ [1 .. length (head (head r))]) <$ [1 .. length (head r)] in a : r ++ [a]) e2
      a = ((False <$ [1 .. length (head (head (head e3)))]) <$ [1 .. length (head (head e3))]) <$ [1 .. length (head e3)]
   in a : e3 ++ [a]

step3 :: [[[Bool]]] -> [[[Bool]]]
step3 = map (map (map (check . count3))) . neighs3 . extendGrid3

step4 :: [[[[Bool]]]] -> [[[[Bool]]]]
step4 = map (map (map (map (check . count4)))) . neighs4 . extendGrid4

part1 :: IO ()
part1 = do
  dat <- loadData
  let res = step3 $ step3 $ step3 $ step3 $ step3 $ step3 dat
  print $ sum $ map (sum . map (\x -> sum [1 | True <- x])) res

part2 :: IO ()
part2 = do
  dat <- return <$> loadData
  let res = step4 $ step4 $ step4 $ step4 $ step4 $ step4 dat
  print $ sum $ map (sum . map (sum . map (\x -> sum [1 | True <- x]))) res