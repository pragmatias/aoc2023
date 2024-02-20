package exos

import fr.pragmatias.tools.Utils
import scala.collection.mutable

class Day12() :

  val dataFileS1 = "day12_step1.txt"
  val dataFileFull = "day12.txt"

  val regexDecim = """(\d+)""".r

  val cache = mutable.Map.empty[(List[Char],List[Int],Int),Long]

  def unfoldAndReplaceSequence(s : String, c : List[Int]) : Long =
    val ns : List[Char] = (1 to 5).map(_ => s).mkString("?").toList
    val nc : List[Int] = (1 to 5).flatMap(_ => c).toList
    return count(ns,nc,0)
    

  //memoization with cache (mutable.Map (function call))
  def count(s: List[Char],c:List[Int],seq:Int) : Long = 
    return cache.getOrElseUpdate((s,c,seq),countNotCached(s,c,seq))

  final def countNotCached(s : List[Char], c : List[Int], seq: Int) : Long = 
    if (s.length == 0) {
      if ((!c.isEmpty && c.length == 1 && c.head == seq)
        || (c.isEmpty && seq == 0)) {
       return 1
      } else {
        return 0
      } 
    } else {
      def operCase() : Long = {
        if (seq == 0) {
          return count(s.tail,c,0)
        } else if (!c.isEmpty && c.head == seq) {
          return count(s.tail,c.tail,0)
        } else {
          return 0
        }
      } 
      def dmgCase() : Long = {
        if (c.isEmpty || (c.head == seq)) {
          return 0
        } else {
          count(s.tail,c,seq+1)
        }
      }
      s.head match
        case '?' => return operCase() + dmgCase() 
        case '#' => return dmgCase()
        case _ => return operCase()         
    }
  
    


  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val listSpring : List[(List[Char],List[Int])]= data.map(_.split(" ")).map(x => (x(0).toList,regexDecim.findAllIn(x(1)).map(_.toInt).toList)).toList
    val res = listSpring.map((a,b) => Utils.getTime(a,debug,count(a,b,0)))
    if (debug) {
      println(listSpring)
      println(res)
    }
    return res.foldLeft(0L)((c,e) => c+e)

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val listSpring : List[(String,List[Int])]= data.map(_.split(" ")).map(x => (x(0),regexDecim.findAllIn(x(1)).map(_.toInt).toList)).toList
    val res = listSpring.map((a,b) => Utils.getTime(a.toList,debug,unfoldAndReplaceSequence(a,b)))
    if (debug) {
      println(listSpring)
      println(res)
    }
    return res.foldLeft(0L)((c,e) => c+e)


@main def runDay12() = 
  val day = Day12()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  println("Step2 : Sample")
  println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step2 : Full")
  // println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
