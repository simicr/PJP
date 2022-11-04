suma::[[Int]] -> [Int]
suma lista = map (foldl (+) 0) lista