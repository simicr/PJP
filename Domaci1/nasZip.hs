-- nasa varijanta zip-a
-- sumira liste 
zip':: [Int] -> [Int] -> [Int] 
zip' lista [] = lista
zip' [] lista = lista
zip' (glava1:rep1) (glava2:rep2) = (glava1 + glava2):zip' rep1 rep2

main = print (zip' [1,2,3]  [1,2])
