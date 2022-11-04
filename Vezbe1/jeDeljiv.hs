-- Proverava da li a deli b.
jeDeljiv:: Int -> Int -> Bool
jeDeljiv a b = (b 'mod' a) == 0

-- Proverava da li je broj deljiv sa 3.
jeDeljiv3:: Int -> Bool
jeDeljiv3 a = jeDeljiv 3 a

