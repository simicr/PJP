import Data.Char
-- Definisati funkciju koja prima listu brojeva i ukoliko je
-- ona parne duzine, svaki element kvadrira, inace svaki element
-- mnozi sa 10. Koristiti funkciju map i lambda funkcije.
zadatak1::[Int] ->[Int]
zadatak1 lista | parno = map (\ x -> x*x ) lista
               | otherwise = map (\ x -> 10 * x) lista
    where
        parno = (mod (length lista) 2) == 0 

-- Napisati funkciju koja prima niz karaktera. Prvo je potrebno
-- funkcijom filter izbaciti sva velika slova, a zatim pomocu funkcije
-- map pretvoriti sva preostala slova u velika.

zadatak2::[Char] -> [Char]
zadatak2 lista = map (toUpper) (filter (not . jeMalo) lista)

jeMalo:: Char -> Bool
jeMalo slovo = slovo >= 'a' && slovo <= 'z'

--  Napisati funkciju "primeni" koja prima 3 parametra:
--	1) f1 :: [[Int]] -> [[Int]] - funkciju koja prihvata [[Int]] i vraca [[Int]]
--	2) f2 :: [[Int]] -> [Int] - funkciju koja prihvata [[Int]] i vraca [Int]
--	3) listu ciji su elementi liste brojeva, tj. [[Int]]
--	Funkcija treba da primeni funkcije f1 i f2 nad prosledjenom listom
--	i vrati rezultat (listu tipa [[Int]] pretvara u [Int], pomocu 
--	funkcija f1 i f2).

primeni::([[Int]] -> [[Int]]) -> ([[Int]] -> [Int]) -> [[Int]] -> [Int]
primeni f1 f2 lista = (f2 . f1) lista

-- Definisati funkciju "izbaciParnePaSumiraj" preko funkcije "primeni"
-- i funkcija iz zadataka 2 i 3 (sa Vezbi 2), koristeci parcijalnu 
-- primenu funkcija.

-- Izbaci elemente deljive sa 2 iz liste
bezParnih::[[Int]] -> [[Int]]
bezParnih lista = map (filter (not . even) ) lista

-- Listu lista brojeva pretvori u listu suma elemenata liste
-- [[1,2,3], [4] , [5,6,7]] -> [6,4,18]
suma::[[Int]] -> [Int]
suma lista = map (foldl (+) 0) lista

zadatak4::[[Int]] -> [Int]
zadatak4 lista = primeni bezParnih suma lista

-- Definisati funkciju prosecnaDuzina koja prima niz stringova
-- i racuna njihovu prosecnu duzinu pomocu funkcije fold i map.
-- Dovoljno je vratiti celobrojnu vrednost (npr. 5 umesto 5.3)

prosecnaDuzina::[[Char]] -> Int
prosecnaDuzina lista = div (foldr (+) 0 (map length lista)) (length lista)