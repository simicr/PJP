-- Izbacuje poslednji element liste.
bezPoslednjeg [] = []
bezPoslednjeg [x] = []
bezPoslednjeg (glava:rep) = glava:bezPoslednjeg (rep)
