package exos

import fr.pragmatias.tools.Utils
import scala.collection.mutable.HashMap
import scala.annotation.tailrec
import scala.compiletime.ops.double

class Day15() :

  val dataFileS1 = "day15_step1.txt"
  val dataFileFull = "day15.txt"

  enum Oper:
   case Store, Remove
  case class Lens(label:String,op:Oper,focal:Int,hash:Int) :
    override def toString() : String = "Lens[%s %s]".format(label,focal)

  def extractLens(s:String) : Lens =
    if (s.contains("=")) then
      val l : Array[String] = s.split("=")
      return Lens(l(0),Oper.Store,l(1).toInt,getHash(l(0)))
    else 
      val l : Array[String] = s.split("-")
      return Lens(l(0),Oper.Remove,0,getHash(l(0)))


  def manageBox(lens : Array[Lens]) : Long = 
    val boxes : HashMap[Int,List[Lens]] = HashMap.empty

    for l <- lens do 
      val getContent = boxes.get(l.hash).getOrElse(List())
      l.op match 
        case Oper.Remove => boxes(l.hash) = getContent.filter(x => x.label != l.label)
        case Oper.Store => 
          if (getContent.filter(x => x.label == l.label).length == 0) then
            boxes(l.hash) = getContent ::: List(l)
          else
            boxes(l.hash) = getContent.map(x => if (x.label == l.label) then l else x)
    
    return getResultFromBox(boxes)
   


  def getResultFromBox(b : HashMap[Int,List[Lens]]) : Long =
    var res : Long = 0L
    for n <- b.keys do
      val l = b.get(n).getOrElse(List())
      for i <- (0 until l.length).toIterator do
        res += ((n+1) * (i+1) * l(i).focal) 
    return res  

  def getHash(s : String) : Int = getHash(s.toList,0)
  @tailrec
  final def getHash(s : List[Char], curr : Int) : Int =
    if (s.isEmpty) then 
      return curr
    else 
      val nAscii = s.head.toInt
      val nCurr = ((curr+nAscii)*17)%256
      return getHash(s.tail,nCurr)




  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val listData : Array[String] = data.head.split(",")
    val res = listData.map(lc => getHash(lc)).sum
    return res

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val listData : Array[Lens] = data.head.split(",").map(extractLens)
    val res = manageBox(listData)
    return res


@main def runDay15() = 
  val day = Day15()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
