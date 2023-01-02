/*
    1. Napisati funkciju koja prima listu tacaka (tacka je par Double-ova)
    i jos jednu "centralnu" tacku (par Double-ova). Iz liste vraca sve tacke
    koja su od centralne tacke udaljene manje od neke prosledjene distance.

    2. Napisati funkciju koja privhata listu Stringova, 
    razdvoji svaki po razmaku, te ih sve
    spoji zarezima. Koristiti map i reduce.

    3. Napisati funkciju koja prima niz brojeva, one koji su parni uveca za 1,
    a neparne kvadrida. Zatim, svaki od brojeva "okrece" (1234 -> 4321) i 
    na kraju izbacuje sve neparne.

    4. Napisati funkciju koja ocekuje listu Int-ova i jos Int n. Za svaki od ostataka
    pri deljenju brojem n, funkcija treba da vrati koliko brojeva iz liste daje taj 
    ostatak. Potrebno je vratiti listu parova oblika:
   
    (ostatak, koliko brojeva iz liste daje ovaj ostatak)

    Primer: List(1, 3, 5, 6, 9), n = 3

    (0, 3),
    (1, 1),
    (2, 1)

    5. Definisati apstraktnu klasu Naselje. Naselje moze da bude Selo, Varosica ili Grad (case klase).
    Sva 3 tipa naselja mogu imati broj stanovnika (integer) i povrsinu (double). 
    Selo nosi informaciju o tome da li je "zbijeno" ili "razbijeno" (string). Grad 
    ima dodatni parametar koji kaze da li sadrzi gradski bazen ili ne (boolean).

    6. Napisati funkciju koja iz liste Naselja izdvaja sva razbijena sela i sve
    gradove sa bazenima koji imaju vise od 150 000 stanovnika.
*/

abstract class Naselje(stanovnici:Int, povrsina:Double)
case class Selo(stanS:Int, povS:Double, tip:String) extends Naselje(stanS, povS)
case class Grad(stanG:Int, povG:Double, bazen:Boolean) extends Naselje(stanG, povG)

object Main {
    def prviZadatak(input:List[(Double, Double)], centralna:(Double, Double), granica:Double):List[(Double, Double)] = {
        input.filter((x) => Math.sqrt((x._1 - centralna._1)*(x._1 - centralna._1) + (x._2 - centralna._2)*(x._2 - centralna._2)) <= granica)
    }
    def drugiZadatak(input:List[String]):String = {
        input.map(x => x.split(" ")).map(x => x.reduce((lista, elem) => lista++","++elem)).reduce((x,y) => x++","++y)
    }
    def treciZadatak(input:Array[Int]):Array[Int] = {
        input.map(x => transf1(x)).map(x => okreni(x, 0)).filter(x => x%2 ==0)
    }

    def transf1(x:Int):Int = {
        var res = x;
        if (x % 2 == 0) {
            res = x + 1
        } else {
            res = x*x
        }
        res
    }

    def okreni(x:Int, res:Int):Int = {
        if(x < 10) {
            return 10*res + x
        } else {
            val cifra = x % 10
            val ostatak = x / 10
            return okreni(ostatak, 10*res+cifra)
        }
    }

    def cetvrtiZadatak(input:List[Int], n:Int):List[(Int, Int)] = {
        for (i <- (0 until n).toList ) 
            yield (i, input.filter(x => x%n ==i).size )
    }

    def sestiZadatak(input:List[Naselje]):List[Naselje] = input.filter(x => x match { case Selo(_, _, "razbijeno") => true case Grad(stanS, _, true) => stanS >150000 case _ => false })


    def main(args:Array[String]):Unit = {
        println(prviZadatak(List((1,1), (2,2), (3,3)), (0,0), 3))
        println(drugiZadatak(List("Ana voli", "Milovana test")))
        println(treciZadatak(Array(1,2,3,4,5,6)).mkString(" "))
        println(cetvrtiZadatak(List(1,2,3,4,5,5), 6))
        println(sestiZadatak(List(Selo(1,1,"razbijeno"), Selo(1,1,"zbijeno"), Grad(150001, 9, true), Grad(150001, 9, false))))
    }
}