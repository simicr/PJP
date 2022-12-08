-- Izbacuje iz liste sve reci koje sadrze samo mala slova.
-- sveMala - Proverava da li je data rec malo slovo.
-- jeMalo - Proverava da li data rec ima samo mala slova.
jeMalo:: Char -> Bool
jeMalo slovo = slovo >= 'a' && slovo <= 'z'

svaMala:: [Char] -> Bool
svaMala rec = all (jeMalo) rec

bezMalihReci:: [[Char]] -> [[Char]]
bezMalihReci lista = filter (svaMala) lista

-- Izbacuje poslednji element liste.
bezPoslednjeg [] = []
bezPoslednjeg [x] = []
bezPoslednjeg (glava:rep) = glava:bezPoslednjeg (rep)

-- Izbacuje pretposlednji element liste.
bezPretposlednjeg [] = []
bezPretposlednjeg [x] = []
bezPretposlednjeg (x:y:[]) = y:[]
bezPretposlednjeg (glava:rep) = glava: bezPretposlednjeg (rep)

-- Proverava da li a deli b
jeDeljiv:: Int -> Int -> Bool
jeDeljiv a b = (mod b a) == 0

-- Proverava da li je a deljiv sa 3
jeDeljiv3:: Int -> Bool
jeDeljiv3 a = jeDeljiv 3 a

-- Nasa rekurzivna implementacija filter funkcije
filter':: (a -> Bool) -> [a] -> [a]
filter' uslov [] = []
filter' uslov (glava:rep) | (uslov glava) = glava:(filter' uslov rep)
                          | otherwise = filter' uslov  rep

-- Vraca sve brojeve deljive sa 3
deljiviSa3:: [Int] -> [Int]
deljiviSa3 lista = filter' jeDeljiv3 lista

-- Racuna n! rekurzivno.
fact 1 = 1
fact n = n * fact (n - 1)

-- Racuna n! repno rekurzivno.
factRep n = akum n 1
akum 1 rez = rez
akum n medju = akum (n - 1) (n*medju)

-- Proverava da li dat string ima samo velika slova.
imaVelikaSlova [] = False
imaVelikaSlova (glava:rep) = if (veliko glava) then True else (imaVelikaSlova rep)

veliko slovo = slovo >= 'A' && slovo <= 'Z'

-- Vraca listu sa kvadriranim elementima.
listaNaKvadrat:: [Int] -> [Int]
listaNaKvadrat lista = [x*x | x <- lista]

-- Brise susede koji su jednaki u jednu listu.
-- Primer: [1,2,2,3,4,4,4,5,5] -> [1,2,3,4,5]
-- [1,2,3,2] -> [1,2,3,2]
spoljsti [] = []
spoljsti (glava:rep) = glava:(spoljsti ( pomocna glava rep))


pomocna broj [] = []
pomocna broj (glava:rep)  | broj == glava = (pomocna broj rep)
                          | otherwise = (glava:rep)
