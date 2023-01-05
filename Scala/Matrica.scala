/*
    Funkcija za mnozenje dve matrice. Ako su lose dimenzije vraca Nil.

    Funkcija koja transponuje matricu. 

    Funkcija koja proverava da li je matrica dijagonalna. 

    Funkcija koja racuna determinantu matrice.
*/

object Main {

    def pomnozi(a:List[List[Int]], b:List[List[Int]]):List[List[Int]] = {
        if(a(0).size != b.size) {
            return Nil
        } else {
            a.map((red) => red.zip(b)).map(red => red.map(tuple => tuple._2.map((x) => x*tuple._1))).map(red => red.reduce((x,y) => for(i <- (0 until x.size).toList ) yield x(i) + y(i) ))
        }
    }

    def transponuj(matrica:List[List[Int]]):List[List[Int]] = {
        val brojRedova = matrica(0).size
        (0 until brojRedova).toList.map((i) => matrica.map((red) => red(i)))
    }

    def jeDijagonalna(matrica:List[List[Int]]):Boolean = {
        if(matrica(0).size != matrica.size) {
            return false
        } else {
            matrica == transponuj(matrica)
        }
    }
    
    def bezIndeks(indeks:Int, matrica:List[List[Int]]):List[List[Int]] = {
        val trans = transponuj(matrica)
        var res = ((trans.zip((0 until trans.size).toList)).filter(x => x._2 != indeks).map(x => x._1))
        transponuj(res)  
    }
    def det(matrica:List[List[Int]]):Int = {
        if(matrica(0).size != matrica.size) {
            return -1
        } else {
            if (matrica.size == 2) {
                matrica(0)(0)*matrica(1)(1) - matrica(0)(1)*matrica(1)(0)
            } else {
                var suma = 0
                for(indeks <- 0 until matrica.size) {
                    var znak = if (indeks % 2 == 0 ) { 1 } else { -1 }
                    suma = suma + znak*matrica(0)(indeks)*det(bezIndeks(indeks, matrica).tail)
                }
                suma
            }       
        }
    }

    def main(args: Array[String]): Unit = {
        var a = List(List(1, 0), List(0, 1))
        var b = List(List(1, 3, 3), List(0, 3, 5), List(0,0,1))
        //println(pomnozi(a,b))
        //println(transponuj(b))
        //println(jeDijagonalna(a))
        //println(jeDijagonalna(b))
        println(det(b))
    }

}

