-- Prosledjuje se kvadratna matrica i vraca se njena determinanta
-- Ukoliko matrica nije kvadratna, vraca se 666

det:: [[Int]] -> Int
det matrica 
    | validna matrica  = det2 matrica 0
    | otherwise = 666

validna::[[Int]] -> Bool
validna matrica = and ( map (\x -> (x == (length matrica))) (map (length) matrica) )

det2:: [[Int]] -> Int -> Int
det2 [[a,b] , [c,d]] suma = suma + (a*d - b*c)
det2 (prvi:ostatak) suma = sum [(znak index)*(prvi !! index)*(det2 (bez index ostatak) suma) | index <- [0..((length prvi) - 1)]]
    where 
        znak index = if ((mod index 2) == 0) then 1 else -1

bez:: Int -> [[Int]] -> [[Int]]
bez index matrica = map (bezItog index) matrica

bezItog:: Int -> [Int] -> [Int]
bezItog index lista 
    | index == 0 = tail lista
    | index == ((length lista) - 1) = take ((length lista) - 1) lista
    | otherwise = (take index lista) ++ (drop (index + 1) lista)