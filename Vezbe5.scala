object Main {
    
    /*
        1. Napisati funkciju tipKaraktera koja prima Char, odredjuje da li je
        on veliko, malo ili nije slovo uopste, i vraca poruku kao String.
    */
   def tipKaraktera(znak:Char):String = {
        if ((znak >= 'a') && (znak <= 'z')) "Znak je malo slovo" else if ((znak >= 'A') && (znak <= 'Z')) "Znak je veliko slovo" else "Znak nije slovo"
   }

    /*
        2. Napisati funkciju pomnoziSa koja prima Int i niz Int-ova,
        a kao rezultat vraca prvi broj pomnozen sa svim brojevima
        niza.
    */
   def pomnoziSa(base:Int, niz:Array[Int]): Int = {
        var rez = base
        for (elem <- niz) 
            rez = rez*elem
        rez
   } 

    /*
        3. Napisati funkciju koja kvadrira sve brojeve nekog niza. (Koristiti
        yield).
    */
   def kvadrira(niz:Array[Int]):Array[Int] = { for(elem <- niz) yield elem*elem }

    /*
        4. Napisati funkciju koja prima Char i Int. U while petlji ispisuje
        karakter c, n puta.
    */
    def ponovi(znak:Char, n:Int): Unit = {
        var cnt = 0
        while (cnt < n) {
            cnt +=1
            println(znak)
        }
    }

    /*
        5. Napisati funkciju kalkulator, koja prihvata 2 Int-a i Char
        (+, -, *, /), i pomocu match-case utvrdjuje koju operaciju
        treba da izvrsi. Za nepoznatu operaciju vratiti 0. Ucitati
        podatke od korisnika.
    */
    def kalkulator(broja1:Int, broj2:Int, op:Char):Int = {
        val rez = op match {
            case '-' => broja1 - broj2
            case '+' => broja1 + broj2
            case '*' => broja1*broj2
            case '/' => if(broj2 != 0) broja1/broj2 else 0
            case _ => 0
        }
        rez
    }

    /*
        6. Napisati funkciju koja utvrdjuje da li je prosledjeni argument nekog
        brojevnog tipa (Int, Float...) ili nije.
    */

    def jeBrojevni(test:Any):Boolean = {
        test match {
            case broj:Int => true
            case realan:Float => true
            case boljiRealan:Double => true
            case _ => false
        }

    }

    /*
        7. Napisati funkciju koja prima par Int-ova i broj n,
        vraca novi par gde je prvi element para pomnozen sa n
        a drugom elementu para je dodat broj n.
    */
    def transform(par:(Int, Int), skalar:Int):(Int,Int) = { (par._1*skalar , par._2 + skalar) }

    /*
        8. Napisati funkciju "spljosti" koja radi sa listama.

	    [1, 2, 2, 2, 3, 3, 4, 5, 5, 5] -> [1, 2, 3, 4, 5]
    */

    def spljosti(input:List[Int]):List[Int] = {
        input match {
			case Nil => Nil
			case (x: Int) :: Nil => x :: Nil
			case (x: Int) :: (y: Int) :: tail => 
				if (x == y)
					spljosti(y :: tail)
				else
					x :: spljosti(y :: tail)
		}
    }

    def main(args:Array[String]):Unit = {
        println(tipKaraktera('a'))
        println(pomnoziSa(1, Array(1,2,3,4,5)))
        println(kvadrira(Array(1,2,3,4,5)).mkString(" "))
        ponovi('b', 10)
        println(kalkulator(2,3,'+'))
        println(kalkulator(2,3,'t'))
        println(jeBrojevni(2))
        println(jeBrojevni('a'))
        println(transform( (1,2) , 3))
        println(spljosti( List(1,2,2,2,3,3,4,5,5,5) ))
    }


}