-- Vraca listu delioca broja N

delioci:: Int -> [Int]
delioci n = [delioci | delioci <- [1..n], (mod n delioci) == 0]

main = print (delioci 10)