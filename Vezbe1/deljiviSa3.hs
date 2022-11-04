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

main = print (deljiviSa3 [1,2,3,6,7,9])