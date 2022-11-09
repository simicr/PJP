import System.Random
type Tacka = (Double, Double)

rastojanje:: Tacka -> Tacka -> Double
rastojanje (x1,y1) (x2,y2) = sqrt(abs(x1-x2)*abs(x1-x2) +  abs(y1-y2)*abs(y1-y2))

kmeans::[Tacka] -> Int -> [Int]
kmeans input k = kmeans2 input k [] [1..k]

kmeans2:: [Tacka] -> Int -> [Int] -> [Int] -> [Int]
kmeans2 input k  preIter iter 
                            | preIter == iter = iter
                            | otherwise = kmeans2 input k iter (novoKlasterisanje centri input)
    where 
        centri = [(1.0,1.0)| x <- [1..k]] -- def funkciju koja u odnosu na iter izracuna nove centre 

novoKlasterisanje::[Tacka] -> [Tacka] -> [Int]
novoKlasterisanje centri input = map (najbliza centri 1 0 10000.0) input

najbliza::[Tacka] -> Int -> Int -> Double -> Tacka -> Int
najbliza [] index indexMin trnMin tacka = indexMin
najbliza (g:rep) index indexMin trnMin tacka 
                        | trnMin > trnRastojanje = najbliza rep (index + 1) index trnRastojanje tacka
                        | otherwise = najbliza rep (index + 1) indexMin trnMin tacka
    where 
        trnRastojanje = (rastojanje g tacka) 