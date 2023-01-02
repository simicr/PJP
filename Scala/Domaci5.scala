
object Main{ 

/* 
    1. Definisati funkciju koja prima listu brojeva i ukoliko je
    ona parne duzine, svaki element kvadrira, inace svaki element
    mnozi sa 10. Koristiti funkciju map.
*/
    def fensiStvariSaListom(input:List[Int]):List[Int] = {
        if (input.size % 2 == 1) { input.map(x => 10*x) }
        else { input.map(x => x*x) }
    }
/*
    2. Napisati funkciju koja prima niz karaktera. Prvo je potrebno
    funkcijom filter izbaciti sva velika slova, a zatim pomocu funkcije
    map pretvoriti sva preostala slova u velika.
*/
    def izbaciVelikaPovecaj(input:Array[Char]):Array[Char] = {
        input.filter(x => x >= 'a' && x <= 'z').map(x => x.toUpper)
    }
/*
    3. Definisati funkciju prosecnaDuzina koja prima niz stringova
    i racuna njihovu prosecnu duzinu pomocu funkcije reduce i map.

*/
    def prosecnaDuzina(input:Array[String]):Double = {
        (1.0)*(input.map(x => x.length()).reduce((x,y) => x + y))/(input.size)
    }
/*
    4. Definisati funkciju koja prima niz brojeva. Zadrzi one brojeve
    kojima je prva i poslednja cifra ista, te svaki broj "okrene"
    po principu: "12341" -> "14321". Koristiti map i filter.
*/
    def obrniBroj(x:Int, rez:Int):Int = {
        if (x < 10) {
            10*rez + x
        }
        else { 
            obrniBroj(x/10, 10*rez + (x%10))
        }
    }

    def zadrziNekePaOkreni(input:Array[Int]):Array[Int] = {
        input.filter((x) => (x%10) == (obrniBroj(x,0)%10)).map(x => obrniBroj(x,0))
    }
/*
    5. Definisati funkciju koja prima niz stringova. Treba da izbaciti
    sve one koji imaju vise od 4 velika slova. Nakon toga potrebno
    je na svaki od njih nalepiti sebe u "ogledalu" ("abc" -> 
    "abccba") i na kraju ih sve spojiti u jedan veliki string. 
    Koristiti map, filter, reduce.
*/
    def izbaciOkreniSpojiSve(input:Array[String]):String = {
        input.filter(x => x.size <= 4).map(x => x + (x.reverse)).reduce(_ + _)
    }

    
    def main(args: Array[String]): Unit = {
        println(fensiStvariSaListom(List(1,2,3,4)))
        println(izbaciVelikaPovecaj(Array('A', 'c', 'B', 'v')).mkString(" "))
        println(prosecnaDuzina(Array("Ana", "Voli")))
        println(obrniBroj(1234,0))
        println(zadrziNekePaOkreni(Array(1231, 1, 45, 65, 333)).mkString(" "))
        println(izbaciOkreniSpojiSve(Array("abc", "aaaaa", "abc")))
    }
}