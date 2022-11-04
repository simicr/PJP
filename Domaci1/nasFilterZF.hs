-- filter funkcija definisana preko ZF izraza

filterZF:: (a -> Bool) -> [a] -> [a]
filterZF f lista = [x | x <- lista, f x]

main = print( filterZF (\ x -> (mod x 2) == 0) [1,2,3,4,5,6] )