-- Sumira cifre prosledjenog broj

-- Rekurzivno
sumaR n | n < 10 = n
        | otherwise = (mod n 10) + sumaR (div n 10)

-- Repno rekurzivno
sumaRR n = sumaRRP n 0
sumaRRP n rez | n < 10 = rez + n 
              | otherwise = sumaRRP (div n 10) (rez + (mod n 10))

main = print (sumaRR 154)