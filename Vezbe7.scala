/*
    1. Definisati apstraktnu klasu Skola sa val poljima ime 
    i broj ucenika. Definisati case klase OsnovnaSkola i
    SrednjaSkola koje nasledjuju klasu Skola, primaju potrebne
    parametre i prosledjuju ih konstruktoru nadklase.

    2. Napisati funkciju koja prima listu skola,    
    te za svaku skolu, po tipu, ispise poruku
    "Osnovna/Srednja skola (ime) ima (broj) ucenika."

    3. Napisati funkciju koja ce iz liste skola izbaciti sve
    skole koje imaju manje od 300 ucenika, koristeci funkciju
    filter.
*/
abstract class Skola(val ime: String, val brojU:Int)
case class OsnovnaSkola(val imeO:String, val brojUO:Int) extends Skola(imeO, brojUO)
case class SrednjaSkola(val imeS:String, val brojUS:Int) extends Skola(imeS, brojUS)

/*
    4. Definisati apstraktnu klasu NebeskoTelo sa val poljima
    ime i precnik (u km). Nakon toga definisati case klase Planeta
    i Satelit koji nasledjuju apstraktnu klasu NebeskoTelo. 
    Planeta takodje ima polje "imaVodu", a Satelit ima polje
    "kruziOko" (String) koje sadrzi ime planete oko koje kruzi.

    5. Napisati funkciju koja pomocu filter funkcije vrati samo one 
    Satelite koji se nalaze u orbiti oko planete cije ime se prosledi
    kao parametar.

    6. Napisati funkciju koja vraca sve Planete koje imaju vodu i imaju 
    precnik veci od 5500 km. Takodje, treba da vrati sve satelite sa
    precnikom manjim od 1000 km.
*/

abstract class NebeskoTelo(val ime:String, val precnik:Int)

case class Planeta(val imeP:String, val precnikP:Int, val imaVode:Boolean) extends NebeskoTelo(imeP, precnikP)
case class Satelit(val imeS:String, val precnikS:Int, val kruziOko:String) extends NebeskoTelo(imeS, precnikS)

object Main {

    def fensiIspisivanjeSkola(skola:List[Skola]):Unit = {
        skola.foreach((x) => ispis(x))
    }

    def ispis(x:Skola):Unit = {
        x match {
            case OsnovnaSkola(ime, broj) => println(s"Osnovna skola $ime ima $broj ucenika")
            case SrednjaSkola(ime, broj) => println(s"Srednja skola $ime ima $broj ucenika")
        }
    }

    def nekoFensiFiltirarenjSkola(skola:List[Skola]):List[Skola] = {
        skola.filter(x => x.brojU < 300)
    }

    def satelitiOkoPlaneteCijeJeIme(ime:String, nebtela:List[NebeskoTelo]):List[NebeskoTelo] = {
        nebtela.filter(x => x match { case Satelit(_,_,kruziOko) => kruziOko == ime case _ => false})
    }
    def velikePlaneteSaVodomMaliSateliti(nebtelo:List[NebeskoTelo]):List[NebeskoTelo] = {
        nebtelo.filter(x => x match { case Satelit(_, precnikS, _) => precnikS < 1000 case Planeta(_, precnikP, imaVode) => imaVode && precnikP > 5500})
    }

    def main(args:Array[String]):Unit = {
        val a = OsnovnaSkola("Skolica", 500)
        val b = SrednjaSkola("Medium skolica", 45)
        fensiIspisivanjeSkola(List(a,b))
        println(nekoFensiFiltirarenjSkola(List(a,b)))
        val nebeskaTela = List(Satelit("satelit", 200, "Zemlja"), Planeta("Zemlja", 56000, true), Planeta("Mars", 90000, false), Satelit("satelit2", 3000, "Zemlja"))
        println(satelitiOkoPlaneteCijeJeIme("Zemlja", nebeskaTela))
        println(velikePlaneteSaVodomMaliSateliti(nebeskaTela))
    }
}