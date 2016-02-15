module Cross where

-- quebra um a list em 3
slice :: Int -> Int -> [Int] -> ([Int], [Int], [Int])
slice from to xs =
 let
  (ll, lr) = splitAt from xs
  (ml, mr) = splitAt (to - from) lr
  in (ll, ml, mr)

removeAllElem :: [Int] -> [Int] -> [Int]
removeAllElem [] ys = []
removeAllElem xs [] = xs
removeAllElem (x:xs) ys
 | elem x ys = removeAllElem xs ys
 | otherwise = x : removeAllElem xs ys

cross :: [Int] -> [Int] -> Int -> Int -> [Int]
cross g1 g2 r1 r2 = xs
 where
  (l1, l2, l3) = slice r1 r2 g1
  ls = removeAllElem g2 l2
  xs = take r1 ls ++ l2 ++ drop r1 ls

-- Gene a ser mutado, dois numeros randomicos (se os numeros são iguais, não faz nada)
muta :: [Int] -> Int -> Int -> [Int]
muta l x y
 | x > y = muta l y x
 | x /= y =
   let (l1, l2h:l2t, l3h:l3t) = slice x y l
    in l1 ++ (l3h : l2t) ++ (l2h : l3t)
 | otherwise = l

-- ignore, é getEdgesVal
getVal :: Int -> Int -> [[Int]] -> Int
getVal x y g = (g !! x) !! y

-- g é o grafo
pSize :: [Int] -> [[Int]] -> Int
pSize [] _ = error "não deve ser []"
pSize [a] _= error "deve conter 2 ou mais"
pSize (a:b:xs) g =
 case xs of
  [] -> getVal a b g
  xs -> pSize (b : xs) g + getVal a b g
