-- Racuna n! rekurzivno.
fact 1 = 1
fact n = n * fact (n - 1)

-- Racuna n! repno rekurzivno.
factRep n = pomocna n 1
pomocna 1 rez = rez
pomocna n medju = pomocna (n - 1) (n*medju)

