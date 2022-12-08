-- Izbaci elemente deljive sa 3 iz liste
izbaci::[[Int]] -> [[Int]]
izbaci lista = map (filter (\x -> (mod x 3) /= 0) ) lista

-- Izbaci elemente deljive sa 2 iz liste
bezParnih::[[Int]] -> [[Int]]
bezParnih lista = map (filter (not . even) ) lista

-- Izbaci elemente deljive sa 3 i potom izbaci one koji
-- imaju manje od 5 elemenata
dodatak:: [[Int]] -> [[Int]]
dodatak lista = filter (\ x -> (length x) > 5) ( izbaci lista)

-- Obrne sve reci u listi stringova
inverzija::[[Char]]->[[Char]]
inverzija lista = map (reverse) lista

-- Listu lista brojeva spaja u jednu listu
-- [[1,2,3], [4] , [5,6,7]] -> [1,2,3,4,5,6,7]
ispeglaj::[[Int]] -> [Int]
ispeglaj lista = foldl (++) [] lista

-- Listu lista brojeva pretvori u listu suma elemenata liste
-- [[1,2,3], [4] , [5,6,7]] -> [6,4,18]
suma::[[Int]] -> [Int]
suma lista = map (foldl (+) 0) lista