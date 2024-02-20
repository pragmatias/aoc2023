package exos

import fr.pragmatias.tools.Utils
import scala.annotation.tailrec

class Day13() :

  val dataFileS1 = "day13_step1.txt"
  val dataFileFull = "day13.txt"


  @tailrec
  final def checkMirrorVert(deb:Int,fin:Int,tab:List[String],check : Boolean) : Boolean =
    if (!check || deb < 0 || fin > tab.length-1) then
      return check
    else return checkMirrorVert(deb-1,fin+1,tab,tab(deb) == tab(fin))

  def checkMirrorVertWithSmurge(deb:Int,fin:Int,tab:List[String]) : Boolean = 
    @tailrec
    def checkMirrorVertWithSmurge(deb:Int,fin:Int,tab:List[String],smurge:Int,check : Boolean) : Boolean =
      if (!check || deb < 0 || fin > tab.length-1)  then
        return check && (smurge == 1)
      else 
        val (check,diff) = checkSmurge(deb,fin,tab)
        return checkMirrorVertWithSmurge(deb-1,fin+1,tab,smurge+diff,check)
      return false
    return checkMirrorVertWithSmurge(deb,fin,tab,0,true)

  def checkSmurge(deb:Int,fin:Int,tab:List[String]) : (Boolean,Int) =
    val s1 = tab(deb)
    val s2 = tab(fin)
    var cptDiff : Int = 0
    for (i <- 0 to s1.length-1) {
      if (s1(i) != s2(i)) then cptDiff+=1
    }
    return (cptDiff <= 1, cptDiff)

  def getNbrColMirrorVert(tab : List[String]) : Option[Int] =
    @tailrec
    def getMirrorVertRec(deb : Int) : Option[Int] = 
      if deb >= tab.length-1 
        then return None 
      else if checkMirrorVert(deb,deb+1,tab,true) 
        then return Option(deb + 1)
      else return getMirrorVertRec(deb+1)
    return getMirrorVertRec(0)

  def getMirror(tab : List[String]) : Int = 
    val tabTranspos : List[String] = (0 to tab.head.length-1).map( x => tab.indices.foldLeft("")((c,e) => c+tab(e)(x))).toList
    return getNbrColMirrorVert(tab) match
      case Some(n) => n * 100
      case None => getNbrColMirrorVert(tabTranspos).getOrElse(0)
  
  def getNbrColMirrorVertWithSmurge(tab : List[String]) : Option[Int] =
    @tailrec
    def getMirrorVertRec(deb : Int) : Option[Int] = 
      if deb >= tab.length-1 
        then return None 
      else if checkMirrorVertWithSmurge(deb,deb+1,tab) 
        then return Option(deb + 1)
      else return getMirrorVertRec(deb+1)
    return getMirrorVertRec(0)
   
  def getMirrorWithSmurge(tab : List[String]) : Int = 
    val tabTranspos : List[String] = (0 to tab.head.length-1).map( x => tab.indices.foldLeft("")((c,e) => c+tab(e)(x))).toList
    return getNbrColMirrorVertWithSmurge(tab) match
      case Some(n) => n * 100
      case None => getNbrColMirrorVertWithSmurge(tabTranspos).getOrElse(0)


  def runStep1(p: os.Path, f: String, debug:Boolean) : Int =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val dataExtract : List[List[String]] = data.foldLeft[List[List[String]]](List(List()))((c,e) => {
      if (e.isEmpty) {
        List()::c
      } else {
        (e::c.head)::c.tail
      }
    })
    val dataSorted = dataExtract.reverse.map(_.reverse)
    val res = dataSorted.map(x => Utils.getTime(x,debug,getMirror(x))).sum
    return res

  def runStep2(p: os.Path, f: String, debug:Boolean) : Int = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val dataExtract : List[List[String]] = data.foldLeft[List[List[String]]](List(List()))((c,e) => {
      if (e.isEmpty) {
        List()::c
      } else {
        (e::c.head)::c.tail
      }
    })
    val dataSorted = dataExtract.reverse.map(_.reverse)
    val res = dataSorted.map(x => Utils.getTime(x,debug,getMirrorWithSmurge(x))).sum
    return res


@main def runDay13() = 
  val day = Day13()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
