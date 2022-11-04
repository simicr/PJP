-- Proverava da li dat string ima samo velika slova.
imaVelikaSlova [] = False
imaVelikaSlova (glava:rep) = if (veliko glava) then True else (imaVelikaSlova rep)

veliko slovo = slovo >= 'A' && slovo <= 'Z'

