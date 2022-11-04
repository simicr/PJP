-- Vraca listu sa kvadriranim elementima.
listaNaKvadrat:: [Int] -> [Int]
listaNaKvadrat lista = [x*x | x <- lista]

