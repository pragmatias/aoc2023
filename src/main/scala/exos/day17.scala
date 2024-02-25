package exos

import fr.pragmatias.tools.Utils
import scala.annotation.tailrec
import scala.collection.mutable.Map

class Day17() :

  val dataFileS1 = "day17_step1.txt"
  val dataFileFull = "day17.txt"

  enum Dir :
    case U,D,L,R
    def goStraight = this match
      case Dir.U => Dir.U
      case Dir.D => Dir.D
      case Dir.L => Dir.L
      case Dir.R => Dir.R
    def goLeft = this match
      case Dir.U => Dir.L
      case Dir.D => Dir.R
      case Dir.L => Dir.D
      case Dir.R => Dir.U
    def goRight = this match
      case Dir.U => Dir.R
      case Dir.D => Dir.L
      case Dir.L => Dir.U
      case Dir.R => Dir.D
     
    
  class HeatMap(data : List[String]) :
    val map : Array[Array[Int]] = data.map(_.toArray.map(_.toString.toInt)).toArray
    val limX = map.head.length
    val limY = map.length
    override def toString() : String = map.map(l => l.mkString("-")).mkString("\n")
    def getHeat(p:Pos) : Int = map(p.y)(p.x)
    def isInHM(p:Pos) : Boolean =
      return p.x >= 0 && p.y >= 0 && p.x < limX && p.y < limY


  case class Pos(x:Int,y:Int) :
    def move(d:Dir) : Pos = d match
      case Dir.U => Pos(x,y-1)
      case Dir.D => Pos(x,y+1)
      case Dir.L => Pos(x-1,y)
      case Dir.R => Pos(x+1,y)
    

  case class State(p : Pos, d : Dir, rep : Int) :
    def goStraight : State = State(p.move(d),d,rep+1)
    def goLeft : State = 
      val nd = d.goLeft
      State(p.move(nd),nd,1)
    def goRight : State = 
      val nd = d.goRight
      State(p.move(nd),nd,1)
    def nextStates(hm:HeatMap, minRep : Int, maxRep : Int) : List[State] = 
      var ns : List[State] = List()
      if (rep < minRep) then ns = List(goStraight)
      else if (rep >= maxRep) then ns = List(goLeft,goRight)
      else if (rep >= minRep && rep < maxRep) then ns = List(goLeft,goRight,goStraight)
      return ns.filter(s => hm.isInHM(s.p))



  def findPath(hm : HeatMap, minRep : Int, maxRep : Int) : Long  =
    val minHeat : Map[State,Long] = Map.empty[State,Long]
    val pFinal : Pos = Pos(hm.limX-1,hm.limY-1)
    val startState = State(Pos(0,0),Dir.R,0)
    minHeat(startState) = 0

    @tailrec
    def findPath(ls : List[State]) : Long =
      if (ls.isEmpty) then 
         return 0L
      else
        val lss = ls.sortBy(minHeat)
        val s = lss.head
        if (s.p == pFinal) then 
          return minHeat(s)
        else 
          val ns = s.nextStates(hm,minRep,maxRep).filterNot(minHeat.contains)
          ns.foreach(x => {
            minHeat(x) = minHeat(s)+hm.getHeat(x.p)
          })
          return findPath(ns:::lss.tail)

    return findPath(List(startState))
    

  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val hm : HeatMap = HeatMap(data.toList)
    val res = Utils.getTime(hm,debug,findPath(hm,1,3))
    return res

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val hm : HeatMap = HeatMap(data.toList)
    val res = Utils.getTime(hm,debug,findPath(hm,4,10))
    return res


@main def runDay17() = 
  val day = Day17()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
