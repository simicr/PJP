-- Izbacujemo svakinti element liste
svakiNti::[a] -> Int -> [a]
svakiNti lista n = pomocni lista n 1

pomocni:: [a] -> Int -> Int -> [a]
pomocni [] n indeks = []
pomocni (glava:rep) n indeks | (mod indeks n) == 0 = pomocni rep n (indeks + 1)
                             | otherwise = glava:pomocni rep n (indeks + 1)

main = print (svakiNti [1,2,3,4,5] 2)