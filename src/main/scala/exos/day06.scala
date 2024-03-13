package fr.pragmatias.exos

import fr.pragmatias.tools.Utils
import scala.annotation.tailrec

class Day06() :

  val dataFileS1 = "day06_step1.txt"
  val dataFileFull = "day06.txt"


  val rexDecim = """(\d+)""".r

  def extractInput(line:String) : List[Long] =
    rexDecim.findAllIn(line.split(":")(1).trim).map(_.toLong).toList

  def extractInputStep2(line:String) : Long = 
    line.split(":")(1).replaceAll(" ","").toLong

  def combineList[A](l1:List[A],l2:List[A]) : List[(A,A)] =
    if (l1.length > 0) {
      (l1.head,l2.head) :: combineList(l1.tail,l2.tail) 
    } else {
      List()
    }


  def findBetterDistance(data:(Long,Long),startHold:Long,maxHold:Long) : Long = 
    val (time,dist) = data 
    return findBetterDistance(time,dist,startHold,maxHold,0L)

  @tailrec
  private def findBetterDistance[A](time:Long,dist:Long,startHold:Long,maxHold:Long,result:Long) : Long =
    if ((startHold >= time) || (startHold > maxHold)) {
      return result
    } else {
      val calc = startHold*(time-startHold)
      if (calc > dist) {
        findBetterDistance(time,dist,startHold+1L,maxHold,result+1L)
      } else {
        findBetterDistance(time,dist,startHold+1L,maxHold,result)
      }
    }


  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : IndexedSeq[String] = os.read.lines(p / f)
    val values = combineList(extractInput(data(0)),extractInput(data(1)))    
    val res = values.map(x => findBetterDistance(x,1L,x._1)).foldLeft(1L)((c,e)=>c*e)
    if (debug) {
      println("Values : "+values)
      println("Resultat : "+res)
    }
    return res

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : IndexedSeq[String] = os.read.lines(p / f)
    val values = (extractInputStep2(data(0)),extractInputStep2(data(1)))   
    // val res = findBetterDistance(values,14L,71516L)
    val res = findBetterDistance(values,1L,values._1)

    if (debug) {
      println("Values : "+values)
      println("Resultat : "+res)
    }
    return res


@main def runDay06() = 
  val day = Day06()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  println("Step2 : Sample")
  println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,true))
  
