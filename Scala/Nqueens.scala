/*
    Resenje problema N kraljica.
*/

object Main {

    
    def jeValidno(konf:List[Int], nivo:Int,pot:Int):Boolean = {
        if(nivo == 0) {
            return true
        }

        val doSada= konf.take(nivo)

        doSada.zip((0 until nivo).toList).forall(x => (x._1) != pot && (Math.abs(x._1 - pot) != Math.abs(x._2 - nivo)))
    }

    def nqueens(trenutno:List[Int], nivo:Int, n:Int):List[List[Int]] = {
        if(nivo == n) {
            return List(trenutno)
        }

        var res:List[List[Int]] = Nil
        for(indeks <- (0 until n).toList if jeValidno(trenutno,nivo,indeks)) {  
            res = res:::nqueens(trenutno:::List(indeks), nivo + 1, n)
        }
        res
        
    }
    def nqueens(n:Int):List[List[Int]] = {
       nqueens(Nil, 0, n)
    }

    def main(args: Array[String]): Unit = {

        println(nqueens(4))
    }
}