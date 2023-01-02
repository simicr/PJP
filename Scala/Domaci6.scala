/*
    1. Definisati apstraktnu klasu Stednja koja predstavlja stedni racun u banci. 
    Stednja moze da bude DinarskaStednja ili DeviznaStednja (case klase). Obe 
    stednje kao osobine imaju kamatnu stopu (double) i informaciju da li su 
    orocene ili neorocene (boolean). DeviznaStednja, za razliku od Dinarske, 
    takodje ima boolean koji predstavlja da li je u evrima ili nije. 

    2. Napisati funkciju koja iz liste Stednji vraca dinarske stednje koje
    koje imaju kamatnu stopu vecu od 2.

    3. Napisati funkciju koja iz liste stednji vraca dinarske stednje koje su
    orocene i devizne stednje koje su u evrima.

    4. Napisati funkciju koja ocekuje listu stednji i racuna prosek kamatnih stopa
    stednji koje su orocene.
*/

abstract class Stednja(stopa:Double, orocena:Boolean)
case class DinarskaStednja(dinarskaStopa:Double, dinarskaOrocena:Boolean) extends Stednja(dinarskaStopa, dinarskaOrocena)
case class DeviznaStednja(deviznaStopa:Double, deviznaOrocena:Boolean, uEvrima:Boolean) extends Stednja(deviznaStopa, deviznaOrocena)


object Main {

    def drugiZadatak(input:List[Stednja]):List[Stednja] = input.filter((x) => x match { case DinarskaStednja(stopa, _) => stopa > 2  case _ => false })

    def treciZadatak(input:List[Stednja]):List[Stednja] = input.filter((x) => x match { case DeviznaStednja(_, _, true) => true case DinarskaStednja(_, true) => true case _ => false })

    def cetvrtiZadatak(input:List[Stednja]):Double = {
        val sumaStopa = input.filter((x) => x match {
            case DinarskaStednja(_, true) => true
            case DeviznaStednja(_,true,_) => true
            case _ => false 
        }).map(x => x match {
            case DinarskaStednja(stopa, _) => stopa
            case DeviznaStednja(stopa, _, _) => stopa 
        }).reduce((x,y) => x + y)

        val brojStopa = input.filter(x => x match {
            case DinarskaStednja(_, true) => true
            case DeviznaStednja(_,true,_) => true
            case _ => false 
        }).size

        sumaStopa/brojStopa
    }

    def main(args: Array[String]): Unit = {
        val stednje = List(DinarskaStednja(3, true), DinarskaStednja(3, false), DinarskaStednja(1, true), DeviznaStednja(1,true,true), DeviznaStednja(1,true,false)
                    , DeviznaStednja(10,false,false))

        println(drugiZadatak(stednje))
        println(treciZadatak(stednje))
        println(cetvrtiZadatak(stednje))
    }
}