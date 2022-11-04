-- Izbacuje pretposlednji element liste.
bezPretposlednjeg [] = []
bezPretposlednjeg [x] = []
bezPretposlednjeg (x:y:[]) = y:[]
bezPretposlednjeg (glava:rep) = glava: bezPretposlednjeg (rep)
