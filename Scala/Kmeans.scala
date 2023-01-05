/*

Potrebno je napisati funkciju koja će da izvrši klasterizovanje (grupisanje) liste tačaka L
u K klastera (grupa) pomoću K-Means algoritma. Ovaj algoritam se izvodi u više
iteracija. Počev od nekih K centralnih tačaka (centri klastera), algoritam izvodi sledeće
korake:
- svakoj tački iz L pronađe najbližu od centralnih tačaka. Tačka pripada onom
klasteru čijem centru je najbliža.
- nakon što je svakoj tački iz L dodeljen klaster, računaju se nove centralne tačke.
Novi centar svakog klastera računa se na osnovu svih tačaka iz L koji pripadaju
tom klasteru. Centar klastera​ i​ u ​narednoj i​ teraciji računa se kao suma svih tačaka klastera ​i
​u trenutnoj iteraciji podeljena sa brojem tačaka koji pripadaju tom klasteru.
Algoritam se završava kada između 2 iteracije ne dođe do promene u klasterima. Za
početne centre klastera uzeti K nasumičnih tačaka iz liste tačaka L.
Glavna funkcija očekuje listu tačaka L (tačka = par Double-ova), kao i neko K koje
predstavlja broj klastera na koji je potrebno podeliti listu tačaka L.Funkcija vraća listu
parova oblika:
( (Double, Double) , Int ) koji predstavljaju (tacku, broj klastera)

*/

class Tacka(var x:Double,var y:Double) {
    def + (t:Tacka):Tacka = new Tacka(x + t.x, y + t.y)

    def podeli (d:Double):Tacka = new Tacka(x/d, y/d)
    
    override def toString: String = "(" + x + ", " + y + ")"
}

object Main {

    def distance(t1:Tacka, t2:Tacka):Double = Math.sqrt(Math.pow(t1.x-t2.x,2) + Math.pow(t1.y-t2.y,2))

    def generisanjeCentara(tacke:List[Tacka],k:Int ,konfig:List[Int]):List[Tacka] = {
        val tacke_klasteri = tacke.zip(konfig)

        var res:List[Tacka] = List()
        for(klaster <- 0 until k) {
            val pripada_klasteru = tacke_klasteri.filter((x) => x._2 == klaster).map((x) => x._1)
            val tackaSuma = pripada_klasteru.reduce((x,y) => x + y)
            res = res:::List(tackaSuma.podeli(1.0*pripada_klasteru.size))
        }
        res
    }

    def najblize(tacke:List[Tacka],k:Int ,centri:List[Tacka]):List[Int] = {
        val centri_klaster = centri.zip((0 until k).toList)
        var res:List[Int] = List()
        for(tacka <- tacke) {
            var najblizi = centri_klaster.reduce((x,y) => if (distance(tacka,x._1) < distance(y._1, tacka)) { x } else { y })
            res = res:::List(najblizi._2)
        }
        res
    }
    def kmeans(tacke:List[Tacka], k:Int, prethodna:List[Int], nova:List[Int], centri:List[Tacka]):List[(Tacka, Int)] = {
        if (prethodna == nova) {
            tacke.zip(nova)
        } else {

            val sledecaPrethodna = nova;
            val sledeciCentri = generisanjeCentara(tacke, k, nova)
            val sledecaNova = najblize(tacke,k , sledeciCentri)

            kmeans(tacke, k, sledecaPrethodna, sledecaNova, sledeciCentri)
        }
    }

    def kmeans(tacke:List[Tacka], k:Int):List[(Tacka, Int)] = kmeans(tacke, k, List(), (0 until k).toList, tacke.take(k))

    def main(args: Array[String]): Unit = {
        val tacke = List(new Tacka(1.0,-1.0), new Tacka(3.0,-2.0), new Tacka(3.0,3.0), new Tacka(5.0, 8.0))
        println(kmeans(tacke, 4))
    }
}
