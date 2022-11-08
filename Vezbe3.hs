-- definisanje nase liste
data Lista a = Prazan | Cvor a (Lista a) deriving Show

-- Napisati funkciju "kreirajMojuListu" koja prima listu brojeva
-- i od njenih elemenata kreira listu pomocu gore definisanog
-- tipa podataka.

kreirajMojuListu::[Int] -> Lista Int
kreirajMojuListu []  = Prazan
kreirajMojuListu (x:xs) = Cvor x (kreirajMojuListu xs)

-- Napisati funkciju "duzinaListe" koja prima gore kreiranu listu
-- i vrati njenu duzinu.

duzinaListe:: (Lista a) -> Int
duzinaListe Prazan = 0
duzinaListe (Cvor info ostatak) = 1 + duzinaListe ostatak

-- Napisati funkciju "uListi" koja prima element i gore definisanu
-- listu, te vraca Boolean u zavisnosti od toga da li se element
-- nalazi u listi ili ne.

uListi:: Eq(a) => (Lista a) -> a -> Bool 
uListi Prazan x = False
uListi (Cvor info ostatak) x = (x == info) || uListi ostatak x

-- Definisati data tip Planeta, koji moze imati vrednosti Nista
-- ili slog koji sadrzi polja za ime :: String, precnik :: Double
-- i gasovita :: Boolean.

data Planeta = Nista | Planeta {
    ime::String,
    precnik::Double,
    gasovita::Bool
} deriving Show

-- Definisati tip podataka Planete kao listu planeta.

type Sistem = [Planeta]

-- Napisati funkciju "nadjiPoImenu" koja prima String i Planete i 
-- vraca planetu sa datim imenom. U slucaju da je ne nadje, vraca
-- Nista (definisano u data tipu Planeta).

nadjiPoImenu:: String -> Sistem -> Planeta
nadjiPoImenu trazi [] = Nista
nadjiPoImenu trazi  (p:ostatak) = if ((ime p) == trazi ) then p else (nadjiPoImenu trazi ostatak)

-- Napisati funkciju "vratiGasovite" koja prima Planete i vraca
--   Planete, ali samo one koje su gasovite.

vratiGasovite :: Sistem -> Sistem
vratiGasovite sistem = filter (gasovita) sistem
