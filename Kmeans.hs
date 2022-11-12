{-
Potrebno je napisati funkciju koja će da izvrši klasterizovanje (grupisanje) liste tačaka L
u K klastera (grupa) pomoću K-Means algoritma. Ovaj algoritam se izvodi u više
iteracija. Počev od nekih K centralnih tačaka (centri klastera), algoritam izvodi sledeće
korake:
- svakoj tački iz L pronađe najbližu od centralnih tačaka. Tačka pripada onom
klasteru čijem centru je najbliža.
- nakon što je svakoj tački iz L dodeljen klaster, računaju se nove centralne tačke.
Novi centar svakog klastera računa se na osnovu svih tačaka iz L koji pripadaju
tom klasteru. Centar klastera​ i​ u ​narednoj i​ teraciji računa se kao suma svih tačaka klastera ​i
​u trenutnoj iteraciji podeljena sa brojem tačaka koji pripadaju tom klasteru.
Algoritam se završava kada između 2 iteracije ne dođe do promene u klasterima. Za
početne centre klastera uzeti K nasumičnih tačaka iz liste tačaka L.
Glavna funkcija očekuje listu tačaka L (tačka = par Double-ova), kao i neko K koje
predstavlja broj klastera na koji je potrebno podeliti listu tačaka L.Funkcija vraća listu
parova oblika:
( (Double, Double) , Int ) koji predstavljaju (tacku, broj klastera)
-}


type Tacka = (Double, Double)

-- Vraca rastojanje izmedju dve tacke
rastojanje:: Tacka -> Tacka -> Double
rastojanje (x1,y1) (x2,y2) = sqrt(abs(x1-x2)*abs(x1-x2) +  abs(y1-y2)*abs(y1-y2))

-- Glavna funkcija
kmeans::[Tacka] -> Int -> [(Tacka, Int)]
kmeans input k = kmeans2 input k [] [1..k] (take k input) 

-- Funkcija koja zapravo izracunava
kmeans2:: [Tacka] -> Int -> [Int] -> [Int] -> [Tacka] -> [(Tacka, Int)]
kmeans2 input k  preIter iter centri
                            | preIter == iter = zip input iter
                            | otherwise = kmeans2 input k iter noviIter noviCentri
    where 
         noviIter = novoKlasterisanje centri input
         noviCentri = recentriranje input noviIter k
         

-- Deo programa sa kojim cemo odrediti nove centre
recentriranje::[Tacka] -> [Int] -> Int -> [Tacka]
recentriranje input iter k = [( (sumaX parovi klaster)/(broj klaster iter), (sumaY parovi klaster)/(broj klaster iter) ) | klaster <- [1..k] ] 
    where 
        parovi = zip input iter

-- Uzece listu parova (Tacka, Int) filtirace one koji pripadaju istom klasteru koji je dat i sumira X komponente.
sumaX:: [(Tacka, Int)] -> Int -> Double
sumaX parovi klaster = sum (map (fst . fst) (filter (\x -> (snd x) == klaster) parovi))
-- Uzece listu parova (Tacka, Int) filtirace one koji pripadaju istom klasteru koji je dat i sumira Y komponente.
sumaY:: [(Tacka, Int)] -> Int -> Double
sumaY parovi klaster =  sum (map (snd . fst) (filter (\x -> (snd x) == klaster) parovi))
-- Vraca koliko ima tacaka u istom klasteru, stim da se vrsi konverzija u Double.
broj:: Int -> [Int] -> Double
broj klaster iter = fromIntegral (length (filter (klaster ==) iter))

-- Deo programa sa kojim cemo izracuniati kom klasteru pripada tacka 
novoKlasterisanje::[Tacka] -> [Tacka] -> [Int]
novoKlasterisanje centri input = map (najbliza centri 1 0 10000.0) input

najbliza::[Tacka] -> Int -> Int -> Double -> Tacka -> Int
najbliza [] index indexMin trnMin tacka = indexMin
najbliza (g:rep) index indexMin trnMin tacka 
                        | trnMin > trnRastojanje = najbliza rep (index + 1) index trnRastojanje tacka
                        | otherwise = najbliza rep (index + 1) indexMin trnMin tacka
    where 
        trnRastojanje = (rastojanje g tacka) 