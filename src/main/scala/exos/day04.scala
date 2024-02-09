package exos

import fr.pragmatias.tools.Utils
import scala.collection.immutable.HashMap


class Day04() :

  val dataFileS1 = "day04_step1.txt"
  val dataFileFull = "day04.txt"

  val regexDecim = """(\d+)""".r

  case class Card(num:Int,listWin : List[Int], listNum: List[Int])
  def getWinNumber(card: Card) : List[Int] =
    return card.listNum.filter(card.listWin.contains(_))

  def countWinPoints(card: Card) : Int = 
    val tmp = getWinNumber(card)
    return if (tmp.length < 2) { tmp.length } else { tmp.tail.foldLeft(1)((c,e) => c*2) }

  // Solution with a mutable hashmap ... not very good
  def listWinScratchpads(listCard: List[Card]) : Int =
    var hashScratch : HashMap[Int,Int] = listCard.map(x => (x.num,1)).foldLeft[HashMap[Int,Int]](HashMap())((c,e) => c+(e._1 -> e._2))
    // println("Before Operation")
    // hashScratch.foreach(println(_))
    listCard.foreach(c => {
      val numWin = getWinNumber(c).length
      val numCard = c.num
      val numScratch = hashScratch.get(c.num).getOrElse(0)
      val bornMax = if (numCard+numWin > listCard.length) { listCard.length } else { numCard+numWin }
      for (e <- List.range(numCard+1,bornMax+1)) {
        val numScratchElt = hashScratch.get(e).getOrElse(0)
        hashScratch += ((e -> (numScratch + numScratchElt)))
      }
      // println("During Operation : Card "+numCard+" --> Win : "+numWin)
      // hashScratch.foreach(println(_))
    })
    // println("After Operation")
    // hashScratch.foreach(println(_))
    return hashScratch.foldLeft(0)((c,e) => c + e._2) 


  // solution with immutable hashmap (better solution)
  def listWinScratchpadsFP(listCard : List[Card], hmScratch : HashMap[Int,Int]) : Int = 
    val c = listCard.head
    val numWin = getWinNumber(c).length
    val numCurrScratch = hmScratch.get(c.num).getOrElse(0)
    
    if (listCard.length > 1) {
      return numCurrScratch + listWinScratchpadsFP(listCard.tail,majScratchpads(hmScratch,numCurrScratch,c.num+1,c.num+numWin))
    } else {
       return numCurrScratch
    }


  def majScratchpads(hmScratch : HashMap[Int,Int],numScratch : Int, start: Int, end: Int): HashMap[Int,Int] = 
    if (start <= end) {
      majScratchpads(hmScratch,numScratch,start+1,end) + (start -> (hmScratch.get(start).getOrElse(0)+numScratch))
    } else {
      hmScratch
    }

  def runStep1(p: os.Path, f: String, debug:Boolean) : Int =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val tmp = data.map(x => x.split(':'))
      .map(x => (x(0),x(1).split('|')))
      .map(x => (x._1,x._2(0),x._2(1)))
      .map(x => Card(regexDecim.findFirstIn(x._1).getOrElse("0").toInt, regexDecim.findAllIn(x._2).filter(!_.isEmpty).map(_.toInt).toList, regexDecim.findAllIn(x._3).filter(!_.isEmpty).map(_.toInt).toList))  
    return tmp.map(x => countWinPoints(x)).foldLeft(0)((c,e) => c+e)

  def runStep2(p: os.Path, f: String, debug:Boolean) : Int = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val tmp = data.map(x => x.split(':'))
      .map(x => (x(0),x(1).split('|')))
      .map(x => (x._1,x._2(0),x._2(1)))
      .map(x => Card(regexDecim.findFirstIn(x._1).getOrElse("0").toInt, regexDecim.findAllIn(x._2).filter(!_.isEmpty).map(_.toInt).toList, regexDecim.findAllIn(x._3).filter(!_.isEmpty).map(_.toInt).toList))  
    
    if (debug) {
      println("Debut Mode On")
      tmp.foreach(println(_))
    }
    // return listWinScratchpads(tmp.toList)
    return listWinScratchpadsFP(tmp.toList, tmp.map(x => (x.num,1)).foldLeft[HashMap[Int,Int]](HashMap())((c,e) => c+(e._1 -> e._2)))


@main def runDay04() = 
  val day = Day04()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
