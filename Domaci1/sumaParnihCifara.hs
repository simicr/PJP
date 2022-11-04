-- suma parnih cifara nekog broja. 


sumaParnih n | n < 10 && jeParan n = n
            | n < 10 = 0
            | suma


jeParan n = ( mod n 2 ) == 0