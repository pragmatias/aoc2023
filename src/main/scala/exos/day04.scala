package exos

import fr.pragmatias.tools.Utils

class Day04() :

  val dataFileS1 = "day04_step1.txt"
  val dataFileS2 = "day04_step2.txt"
  val dataFileFull = "day04.txt"

  val regexDecim = """(\d+)""".r

  case class Card(num:Int,listWin : List[Int], listNum: List[Int])
  def getWinNumber(card: Card) : List[Int] =
    return card.listNum.filter(card.listWin.contains(_))

  def countWinPoints(card: Card) : Int = 
    val tmp = getWinNumber(card)
    return if (tmp.length < 2) { tmp.length } else { tmp.tail.foldLeft(1)((c,e) => c*2) }

  def runStep1(p: os.Path, f: String, debug:Boolean) : Int =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val tmp = data.map(x => x.split(':'))
      .map(x => (x(0),x(1).split('|')))
      .map(x => (x._1,x._2(0),x._2(1)))
      .map(x => Card(regexDecim.findFirstIn(x._1).getOrElse("0").toInt, regexDecim.findAllIn(x._2).filter(!_.isEmpty).map(_.toInt).toList, regexDecim.findAllIn(x._3).filter(!_.isEmpty).map(_.toInt).toList))  
    return tmp.map(x => countWinPoints(x)).foldLeft(0)((c,e) => c+e)

  def runStep2(p: os.Path, f: String, debug:Boolean) : Int = 
    return 0


@main def runDay04() = 
  val day = Day04()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  println("Step1 : Full")
  println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step2 : Full")
  // println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
