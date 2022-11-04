-- Izbacuje iz liste sve reci koje sadrze samo mala slova.
bezMalihReci:: [[Char]] -> [[Char]]
bezMalihReci lista = filter (svaMala) lista

-- Proverava da li data rec ima samo mala slova.
svaMala:: [Char] -> Bool
svaMala rec = all (jeMalo) rec

-- Proverava da li je data rec malo slovo.
jeMalo:: Char -> Bool
jeMalo slovo = slovo >= 'a' && slovo <= 'z'
