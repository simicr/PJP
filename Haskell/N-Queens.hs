-- Resenje problema N kraljica.
-- Tabla je matrica sa 0 i 1

validna::[[Int]] -> Int -> Bool
validna tabla n = and [validanRed tabla n 0 i | i <- [1..(n-1) ]]

validanRed:: [[Int]] -> Int -> Int -> Int -> Bool 
validanRed tabla n prethodni i 
    | prethodni == i = True
    | (mestoP) == (mesto) = False 
    | (abs (mestoP - mesto)) == (abs (i - prethodni)) = False
    | otherwise = validanRed tabla n (prethodni+1) i
    where
        mesto = (snd . head) ( filter (\x -> ((fst x) == 1) ) (zip (tabla !! i) [0 .. (n-1)] )) 
        mestoP = (snd . head) ( filter (\x -> ((fst x) == 1) ) (zip (tabla !! prethodni) [0 .. (n-1)]) )


generisiSve:: Int -> Int -> [[[Int]]]
generisiSve n red
    | red == (n) = [[]] 
    | otherwise = resenje
    where
        mogucnosti = [((take i [0, 0..]) ++ (take (n-i) (1:[0, 0..])))| i <- [0..n-1]] 
        ostatak = (generisiSve n (red + 1))
        resenje = [ [prvi] ++ tabla |prvi <- mogucnosti, tabla <- ostatak]

kraljice:: Int -> [[[Int]]]
kraljice n = filter (\x -> (validna x n)) (generisiSve n 0)