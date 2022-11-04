-- Sumira elemente liste

sumirano:: [Int] -> Int
sumirano lista = foldr (+) 0 lista

main = print( sumirano [1,2,3,4,5,6])