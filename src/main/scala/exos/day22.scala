package exos

import fr.pragmatias.tools.Utils
import scala.util.matching.Regex
import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.runtime.stdLibPatches.language.`3.0`

class Day22() :

  val dataFileS1 = "day22_step1.txt"
  val dataFileFull = "day22.txt"

  val regexBricks : Regex = """^(\d+),(\d+),(\d+)~(\d+),(\d+),(\d+)$""".r

  case class Pos(x:Int,y:Int,z:Int)
  case class Bricks(start:Pos,end:Pos) : 
    def axeCollide(b:Bricks)(axe:Pos => Int) : Boolean =
      val minX = axe(start) min axe(end)
      val maxX = axe(start) max axe(end)
      val otherMinX = axe(b.start) min axe(b.end)
      val otherMaxX = axe(b.start) max axe(b.end)
      return minX <= otherMaxX && maxX >= otherMinX

    def collideWith(b:Bricks) : Boolean = 
      return axeCollide(b)(p => p.x)
              && axeCollide(b)(p => p.y)
              && axeCollide(b)(p => p.z)
              
    def collideWithFloor() : Boolean = 
      val minX = start.z min end.z
      return minX < 1

    def isOn(b:Bricks) : Boolean = 
      val maxX = start.z max end.z
      val otherMinX = b.start.z min b.end.z
      return axeCollide(b)(p => p.x) && axeCollide(b)(p => p.y) && (maxX == (otherMinX + 1))

    def isUnder(b:Bricks) : Boolean = 
      val minX = start.z min end.z 
      val otherMaxX = b.start.z max b.end.z
      return axeCollide(b)(p => p.x) && axeCollide(b)(p => p.y) && ((minX + 1) == otherMaxX)
    
    def moveBricksFall() : Bricks =
      return Bricks(start.copy(z=start.z-1),end.copy(z=end.z-1))

  

  case class Snapshot(listBricks : List[Bricks]) : 

    def fallBricks() : (List[Bricks],HashMap[Bricks,Set[Bricks]])   = 
      val hmBricksUnder : HashMap[Bricks,Set[Bricks]] =  HashMap.empty
      return fallingBricks(listBricks,hmBricksUnder,List())
   
    @tailrec
    private def fallingBricks(lb:List[Bricks],hmBricksUnder:HashMap[Bricks,Set[Bricks]],lres:List[Bricks]) : (List[Bricks],HashMap[Bricks,Set[Bricks]]) =
      if lb.isEmpty then 
        return (lres,hmBricksUnder)
      else 
        val lbSort = lb.sortBy(_.start.z)
        val oldBricks = lbSort.head
        val newBricks = oldBricks.moveBricksFall()
        val lCollide = lres.filter(b => newBricks.collideWith(b)).toSet
        if (lCollide.size > 0 || newBricks.collideWithFloor()) then 
          return fallingBricks(lbSort.tail,HashMap((oldBricks,lCollide))++hmBricksUnder,oldBricks::lres)
        else 
          return fallingBricks(newBricks::lbSort.tail,hmBricksUnder,lres)


    def getBricksWhoCanBeRemoved(hmBricksUnder:HashMap[Bricks,Set[Bricks]]) : List[Bricks] =
      val notRes = hmBricksUnder.filter(_._2.size == 1).map(_._2.head).toSet
      val res = hmBricksUnder.keySet.diff(notRes)
      return res.toList
  
    def getNumberOfBricksFall(b:Bricks,lremoved:List[Bricks],hmBricksUnder:HashMap[Bricks,Set[Bricks]]) : (Bricks,Int) = 
      val hmBricksFallingCalc : HashMap[Bricks,Int] = HashMap.empty
      @tailrec
      def researchBricks(lb:List[Bricks],lres:Set[Bricks]) : Int =
       if lb.length == 0 then
        lres.size
       else 
        val nlres = lb.toSet++lres
        val getNew = hmBricksUnder.filter( x => !x._2.isEmpty && x._2.subsetOf(nlres) && !nlres.contains(x._1)).toList
        return researchBricks(getNew.map(_._1),nlres)
      if (lremoved.contains(b)) then 
        return (b,0)
      else 
        return (b,researchBricks(List(b),Set())-1)


    def calculStep1() : List[Bricks] =
      val (listBricks,hmBricksUnder) = fallBricks()
      return getBricksWhoCanBeRemoved(hmBricksUnder)
     
    def calculStep2() : Int =
      val (listBricks,hmBricksUnder) = fallBricks()
      val lremoved = getBricksWhoCanBeRemoved(hmBricksUnder)
      val res = listBricks.map(b => getNumberOfBricksFall(b,lremoved,hmBricksUnder)).map(_._2)
      println(res)
      return res.foldLeft(0)((c,e)=>c+e)


    def showXZ(lb:List[Bricks]) : String = 
      var res : String = ""
      val limX : Int = lb.map(b => b.start.x max b.end.x).max
      val limZ : Int = lb.map(b => b.start.z max b.end.z).max
      for z <- -limZ to 0 do
        for x <- 0 to limX do
        if z == 0 then 
          res+="-"
        else if (lb.filter(b => b.start.x <= x && b.end.x >= x && b.start.z <= Math.abs(z) && b.end.z >= Math.abs(z)).length >0) then 
          res += "#"
        else 
          res += "."
        res+="\n"
      return res
    
    def showYZ(lb:List[Bricks]) : String = 
      var res : String = ""
      val limZ : Int = lb.map(b => b.start.z max b.end.z).max
      val limY : Int = lb.map(b => b.start.y max b.end.y).max
      for z <- -limZ to 0 do
        for y <- 0 to limY do
        if z == 0 then 
          res+="-"
        else if (lb.filter(b => b.start.y <= y && b.end.y >= y && b.start.z <= Math.abs(z) && b.end.z >= Math.abs(z)).length >0) then 
          res += "#"
        else 
          res += "."
        res+="\n"
      return res



  def extractBricksFromInput(data : List[String]) : List[Bricks] = 
    data.map(x => x match
      case regexBricks(x1,y1,z1,x2,y2,z2) => Bricks(Pos(x1.toInt,y1.toInt,z1.toInt),Pos(x2.toInt,y2.toInt,z2.toInt))
    )

  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val listBricks : List[Bricks] = extractBricksFromInput(data.toList)
    val snap : Snapshot = Snapshot(listBricks)
    val res = snap.calculStep1()
    if debug then 
      // listBricks.foreach(println)
      val (snapFall,_) = Snapshot(listBricks).fallBricks()
      snapFall.foreach(println)
      println("=====\nInput :")
      println(snap.showXZ(listBricks))
      println("=====\nFalled :")
      println(snap.showXZ(snapFall))
      println("")
      println("=====\nInput :")
      println(snap.showYZ(listBricks))
      println("=====\nFalled :")
      println(snap.showXZ(snapFall))
      println("")
    return res.length

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val listBricks : List[Bricks] = extractBricksFromInput(data.toList)
    val snap : Snapshot = Snapshot(listBricks)
    val res = snap.calculStep2()
    return res


@main def runDay22() = 
  val day = Day22()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
