-- Napisati funkciju koja prihvata String i razdvoji ga
-- po nekom karakteru.

rastaviString::Char -> [Char] -> [[Char]]
rastaviString del rec = pomocna del rec [] []

pomocna:: Char -> [Char] -> [Char] -> [[Char]] -> [[Char]]
pomocna del [] akum lista = lista ++ [akum]
pomocna del (g:rep) akum lista 
    | g == del = pomocna del rep [] (lista ++ [akum])
    | otherwise = pomocna del rep (akum ++ [g]) lista


-- Napisati funkciju koja prihvata listu Stringova.
-- Sve stringove spoji tako sto izmedju svaka 2 
-- umetne karakter ','.

spojiSa::Char -> [[Char]] -> [Char]
spojiSa mer [x] = x
spojiSa mer (g:rep) = (g ++ [mer]) ++ (spojiSa mer rep) 

-- Napisati funkciju koja privhata listu Stringova, 
-- razdvoji svaki po razmaku, te ih sve
-- spoji zarezima. Koristiti map, fold i prethodne 2 funkcije.

transformisi::[[Char]] -> [Char]
transformisi rec = spojiSa ',' (foldr (++) [] (map (rastaviString ' ') rec))

-- Napisati funkciju koja prihvata listu listi integer-a ( [[Int]] ).
-- Potrebno je prvo kvadrirati elemente svake podliste, zatim ih sumirati.
-- Na kraju potrebno je vratiti proizvod te liste.

svastaSaListom:: [[Int]] -> Int
svastaSaListom lista = product ((map (sum . map (\x -> x*x)) lista))

data Naselje = Selo{
    stanovnici::Int,
    povrsina::Double,
    tip::[Char]
}| Grad{
    stanovnici::Int,
    povrsina::Double,
    bazen::Bool
} | Varosica{
    stanovnici::Int,
    povrsina::Double
}

filterNaselja::[Naselje] -> [Naselje]
filterNaselja naselja = filter (kriterijum) naselja

kriterijum::Naselje -> Bool
kriterijum (Grad stanovnici _ bazen) = bazen && stanovnici > 150000
kriterijum (Selo _ _ tip) = tip == "razbijena"
