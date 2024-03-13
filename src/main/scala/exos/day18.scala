package fr.pragmatias.exos

import fr.pragmatias.tools.Utils
import scala.annotation.tailrec

class Day18() :

  val dataFileS1 = "day18_step1.txt"
  val dataFileFull = "day18.txt"

  enum Dir :
    case U,D,L,R

  case class Pos(x:Int,y:Int) :
    def getListPos(dig:Dig) : List[Pos] = 
      (1 to dig.meter).foldLeft[List[Pos]](List())((c,e) => getNextPos(dig.dir,e)::c)
    def getNextPos(dir:Dir,len:Int) : Pos = 
      dir match
        case Dir.R => Pos(x+len,y)
        case Dir.L => Pos(x-len,y)
        case Dir.U => Pos(x,y-len)
        case Dir.D => Pos(x,y+len)
      
      
  case class Dig(dir:Dir, meter:Int)
  
  case class DigMap(ld:List[Dig]):
    val m : Array[Array[Char]] = createMap()
    val rangeX = m.head.indices
    val rangeY = m.indices
    override def toString() : String = m.map(x => x.mkString("")).mkString("\n")
    def createMap() : Array[Array[Char]] =
      val (_,lp) : (Pos,List[Pos]) = ld.foldLeft[(Pos,List[Pos])](Pos(0,0),List(Pos(0,0))) :
        case ((pos,lres),dig) => 
          val ltmp = pos.getListPos(dig)
          (ltmp.head,ltmp:::lres) 
      val limX = lp.map(_.x).max+1  
      val limY = lp.map(_.y).max+1 
      val map = Array.ofDim[Char](limY,limX)
      for (x <- 0 to limX-1) do 
        for (y <- 0 to limY-1) do 
          map(y)(x) = if (lp.filter(p => p.x == x && p.y == y).length == 0) then '.' else '#' 
      return map

  def extractArea(ld : List[Dig]) : Long = 
    val (_,area) : ((Long,Long),Long) = ld.foldLeft((0L,0L),1L):
      case(((x,y),area),Dig(dir,len)) => dir match
        case Dir.R => ((x+len,y),area+len)
        case Dir.L => ((x-len,y),area)
        case Dir.U => ((x,y-len),area-(x*len))
        case Dir.D => ((x,y+len),area+((x+1)*len))
    return area

  
  def getDir(s:String) : Dir = s match
    case "U" => Dir.U
    case "D" => Dir.D
    case "L" => Dir.L
    case "R" => Dir.R
    case "0" => Dir.R
    case "1" => Dir.D
    case "2" => Dir.L
    case "3" => Dir.U
    
  def extractDigP1(l : List[String]) : List[Dig] = 
    l.map(x => x.split(" ")).map(x => Dig(getDir(x(0)),x(1).toInt))
  
  def extractDigP2(l : List[String]) : List[Dig] = 
    l.map(x => x.split(" ")).map(x => x(2).slice(2,8)).map(x => Dig(getDir(x.slice(5,6)),Integer.parseInt(x.slice(0,5),16)))

  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val ld = extractDigP1(data.toList)
    val dm = DigMap(ld)
    if debug then
      println(dm)
    return extractArea(ld)

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val ld = extractDigP2(data.toList)
    if debug then
      ld.foreach(println)
    return extractArea(ld)


@main def runDay18() = 
  val day = Day18()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
