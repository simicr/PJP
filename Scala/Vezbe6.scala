object Main {
    /*
    1. Napisati funkciju "ispeglaj" koja prima listu ciji su elementi
   liste List[List[Any]], a vraca listu brojeva List[Any] tako sto sve elemente
   podlisti spoji u jednu listu. (Koristiti reduce)

    [[1, 2, 3], [4, 5], [6], [7, 8], [], [9]] -> [1, 2, 3, 4, 5, 6, 7, 8, 9] */
    def ispeglaj(input:List[List[Any]]):List[Any] = {
        input.reduce((x,y) => x:::y)
    }

    /*
        2. Napisati funkciju koja prima listu listi brojeva List[List[Int]], i vraca
        listu suma elemenata listi koje ona sadrzi. Koristiti map i reduce.

        [[1, 2, 3], [2, 3], [1, 2, 4], [5]] -> [6, 5, 7, 5]
    */
    def prefinjenoIspeglaj(input:List[List[Int]]):List[Int] = {
        input.map((x) => x.sum)
    }    

    /*
        3. Napisati funkciju koja prima listu ciji su elementi liste brojeva,
        te iz svake podliste izbacuje parne brojeve. Ako slucajno ostane
        neka prazna lista, nju je potrebno izbaciti. Koristiti map i 
        filter

        [[1, 2, 3], [2, 4], [3, 4, 5], [7]] -> [[1, 3], [3, 5], [7]]
    
    */
    def izbacivanjeParnih(input:List[List[Int]]):List[List[Int]] = {
        (input.map((x) => x.filter((y) => y % 2 == 1) ) ).filter((x) => x.size != 0)
    }

    /*
        4. Napisati funkciju "okreni" koja prolazi kroz listu stringova, 
        te pomocu funkcija map i reverse "okrene" sve stringove.

        "neki string" -> "gnirts iken"
        ["abc", "dfg", "ert"] -> ["cba", "gfd", "tre"]
    */
    def inverzijaListe(input:List[String]):List[String] = {
        (input.map(x => x.reverse))
    }

    /*
        5. Napisati funkciju koja prihvata listu listi brojeva [[Int]] i 
        iz svake podliste izbacuje elemente deljive sa 3.
    */
    def izbaciDeljiveSa3(input:List[List[Int]]):List[List[Int]] = {
        input.map((x) => x.filter((y) => y % 3 != 0) ) 
    }

    def izbaciDeljiveSa3PaPotomIzbaci(input:List[List[Int]]):List[List[Int]] = {
        (input.map((x) => x.filter((y) => y % 3 != 0))).filter((x) => x.size > 5)
    }

    def main(args:Array[String]):Unit = {
        println(ispeglaj(List(List(1), List(2), List(3))))
        println(prefinjenoIspeglaj(List(List(1,2,3), List(2,4), List(3))))
        println(izbacivanjeParnih(List(List(2), List(1,3), List(2,1,4))))
        println(inverzijaListe(List("abc", "dfg", "ert")))
        println(izbaciDeljiveSa3(List(List(2), List(1,3), List(6,1,3))))
    }
}