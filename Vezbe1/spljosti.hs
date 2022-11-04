-- Brise susede koji su jednaki u jednu listu.
-- Primer: [1,2,2,3,4,4,4,5,5] -> [1,2,3,4,5]
-- [1,2,3,2] -> [1,2,3,2]
spoljsti [] = []
spoljsti (glava:rep) = glava:(spoljsti ( pomocna glava rep))


pomocna broj [] = []
pomocna broj (glava:rep)  | broj == glava = (pomocna broj rep)
                          | otherwise = (glava:rep)

