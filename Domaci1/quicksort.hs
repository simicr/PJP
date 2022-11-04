-- Quicksort implementiran u Haskell-u

quick:: (Ord(a)) => [a] -> [a]
quick [] = []
quick (pivot:ostatak) = quick manji ++ [pivot] ++ quick veci
    where 
        manji = [x | x <- ostatak, x <= pivot]
        veci = [x| x <- ostatak, x > pivot]

main = print (quick [4,2,3,6,1])