{- 
   Kreirati novi data tip Stablo, koji moze imati elemente Nista
   i Cvor, gde Cvor sadrzi nosi neki Int, kao i levo i desno podstablo
   (koji su tipa Stablo). 
-}

data Stablo = Nista | Cvor Int (Stablo) (Stablo) deriving Show

{- 
   Napisati funkciju "sadrzi" koja prima Int i Stablo, te pretrazuje
   da li se prosledjeni Int nalazi u stablu. Vraca Boolean vrednost.
-}

sadrzi::Stablo -> Int -> Bool
sadrzi Nista x = False
sadrzi (Cvor info levo desno) x 
                | x == info = True
                | otherwise = (sadrzi levo x) || (sadrzi desno x)

{-
  Napisati funkciju uListu koja sve brojeve u stablu smesta u listu.
-}

uListu::Stablo -> [Int]
uListu Nista = []
uListu (Cvor x levo desno) = (uListu levo) ++ [x] ++ (uListu desno)

{-
  Napisati funkciju koja vraca listu svih brojeva u stablu
   koji su deljivi sa 3 ili 5.
-}

deljiviSa3ili5::Stablo -> [Int]
deljiviSa3ili5 stablo = filter (\x -> ((mod x 3) == 0) || ((mod x 5) == 0) ) (uListu stablo)

{-
   Napisati funkciju preslikaj koja prima stablo i preslika ga
   "u ogledalu". (Obrne levo i desno podstablo).
-}

obrni:: Stablo -> Stablo
obrni Nista = Nista
obrni (Cvor info levo desno) = (Cvor info (obrni desno) (obrni levo))

{-
   Napisati funkciju filterStablo. Ona kao parametre ocekuje funkciju f
   koja prima Int i vraca Bool, i Stablo. Rezultat funkcije filterStablo
   je lista brojeva iz stabla koji vrate True primenom funkcije f
-}

filterStablo:: (Int -> Bool) -> Stablo -> [Int]
filterStablo f stablo = filter (f) (uListu stablo)

