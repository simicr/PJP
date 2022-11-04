-- Nasa verzija filter funkcije. Postignuta rekuzrzivno.
filter':: (a -> Bool) -> [a] -> [a]
filter' uslov [] = []
filter' uslov (glava:rep) | (uslov glava) = glava:(filter' uslov rep)
                          | otherwise = filter' uslov  rep

main = print (filter' (\ x-> (mod x 2) == 0) [1,2,3,4,5])