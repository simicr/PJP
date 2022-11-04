izbaci::[[Int]] -> [[Int]]
izbaci lista = map (filter (\x -> (mod x 3) /= 0) ) lista

dodatak:: [[Int]] -> [[Int]]
dodatak lista = filter (\ x -> (length x) > 5) ( izbaci lista)