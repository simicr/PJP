
object Main {
    /*
        1. Napisati funkciju koja prebrojava koliko elementa niza 
        je parno.
    */

    def brojParnih(input:Array[Int]):Int = {
        var brojac = 0
        for (elem <- input if (elem%2==0)) {
            brojac = brojac + 1
        }
        brojac
    }
    
    /*
        2. Napisati funkciju koja prihvata niz string-ova i vraca 
        samo one koji su duzi od nekog n.
    */
    
    def izbaciKrace(input:Array[String], n:Int):Array[String] = {
        for (elem <- input if (elem.length() > n)) yield elem
    }

    /*
        3. Napisati funkciju koja prima niz brojeva i vraca sve kombinacije
        parova brojeva tog niza (Tuple2[Int, Int]). Nakon toga, definisati
        funkciju koja iz niza parova vraca samo one cija srednja vrednost
        upada u neki interval [a..b].
    */

    def zip(input:Array[Int]):Array[Tuple2[Int, Int]] = {
        for(elem1 <- input; elem2 <- input) yield (elem1, elem2)
    }

    def zipWith(input:Array[Tuple2[Int,Int]], a:Int, b:Int):Array[Tuple2[Int,Int]] = {
        for ((prvi, drugi) <- input; if((a <= (prvi+drugi)/2) && ( b >= (prvi+drugi)/2))) yield (prvi,drugi)
    }

    /* 
        4. Napisati funkciju koja vraca skalarni proizvod dva niza.
    */

    def dotProduct(input1:Array[Int],input2:Array[Int]):Int = {
        if (input1.length != input2.length) {
            -1
        } else {
            val duzina = input1.length
            var suma = 0
            for(i <- 0 until duzina) {  suma = suma + input1(i)*input2(i) }
            suma
        }
    }
     
    /*
        5. Napisati funkciju koja prebrojava koliko elemenata liste 
        l je manje od nekog n.
    */

    def manjihOd(input:List[Int], n:Int):Int = {
        (input.filter(x => (x < n))).size
    }

    /*
        6. Napisati funkciju koja prihvata listu i vraca listu
        svih prostih brojeva iz prvobitne liste.
    */

    def jeProst(input:Int):Boolean = {
        var res = true;

        for(test <- 2 until input) {
            if ( input % test == 0) {
                res = false;
            }
        
        }
        res
    }

    def prosti(input:List[Int]):List[Int] = {
        (input.filter(x => jeProst(x)))
    }
    
    /*
        7. Napisati quicksort za sortiranje listi.
    */

    def manjiJednako(input:List[Int], target:Int):List[Int] = input.filter((x) => x <= target)
    def vecih(input:List[Int], target:Int):List[Int] = input.filter((x) => x > target)

    def quicksort(input:List[Int]):List[Int] = {
        input match {
            case Nil => Nil
            case x::ostatak => quicksort(manjiJednako(ostatak,x)):::List(x):::quicksort(vecih(ostatak, x))
        }
    }

    def main(args:Array[String]):Unit = {
        println(quicksort(List(2,1,3,1,4,5)))
    }
}