-- Vraca listu delioca broja N
delioci:: Int -> [Int]
delioci n = [delioci | delioci <- [1..n], (mod n delioci) == 0]


-- suma parnih cifara nekog broja. 
jeParan n = ( mod n 2 ) == 0

sumaParnih n 
            | n < 10 && jeParan n = n
            | n < 10 = 0
            | jeParan n = (mod n 10) + sumaParnih (div n 10)
            | otherwise = sumaParnih (div n 10)


-- Izbacujemo svakinti element liste
pomocni:: [a] -> Int -> Int -> [a]
pomocni [] n indeks = []
pomocni (glava:rep) n indeks | (mod indeks n) == 0 = pomocni rep n (indeks + 1)
                            | otherwise = glava:pomocni rep n (indeks + 1)

svakiNti::[a] -> Int -> [a]
svakiNti lista n = pomocni lista n 1


-- filter funkcija definisana preko ZF izraza
filterZF:: (a -> Bool) -> [a] -> [a]
filterZF f lista = [x | x <- lista, f x]


-- Sumira cifre prosledjenog broj
-- a Rekurzivno
-- b Repno rekurzivno

sumaR n | n < 10 = n
        | otherwise = (mod n 10) + sumaR (div n 10)

sumaRR n = sumaRRP n 0
sumaRRP n rez | n < 10 = rez + n 
              | otherwise = sumaRRP (div n 10) (rez + (mod n 10))

-- Sumira elemente liste
sumirano:: [Int] -> Int
sumirano lista = foldr (+) 0 lista

-- Implementacija zip-a
-- sumira liste 
zip':: [Int] -> [Int] -> [Int] 
zip' lista [] = lista
zip' [] lista = lista
zip' (glava1:rep1) (glava2:rep2) = (glava1 + glava2):zip' rep1 rep2

-- Quicksort implementiran u Haskell-u
quick:: (Ord(a)) => [a] -> [a]
quick [] = []
quick (pivot:ostatak) = quick manji ++ [pivot] ++ quick veci
    where 
        manji = [x | x <- ostatak, x <= pivot]
        veci = [x| x <- ostatak, x > pivot]