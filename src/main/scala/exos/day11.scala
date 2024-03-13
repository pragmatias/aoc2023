package fr.pragmatias.exos

import fr.pragmatias.tools.Utils
import scala.annotation.tailrec

class Day11() :

  val dataFileS1 = "day11_step1.txt"
  val dataFileFull = "day11.txt"


  enum TypeTile :
    case Void,Galaxy

  case class Gal(t:TypeTile,p:Pos)
  case class Pos(x:Int,y:Int)
  
  class MapGal(data : List[List[String]]) :
    val gal : List[Gal] = createGal()
    val galLimX : Int = (gal.foldLeft(0)((c,e) => if (c < e.p.x) then e.p.x else c) + 1)
    val galLimY : Int = (gal.foldLeft(0)((c,e) => if (c < e.p.y) then e.p.y else c) + 1)
    val galExt : List[Gal] = createGalExt()
    val galExtLimX : Int = (galExt.foldLeft(0)((c,e) => if (c < e.p.x) then e.p.x else c) + 1)
    val galExtLimY : Int = (galExt.foldLeft(0)((c,e) => if (c < e.p.y) then e.p.y else c) + 1)

    def showGal() : Unit = 
      for (y <- List.range(0,galLimY))
        for ( x <- List.range(0,galLimX))
          gal.filter(_.p == Pos(x,y)).head match 
            case Gal(TypeTile.Galaxy,_) => print("G")
            case Gal(TypeTile.Void,_) => print(".")
            case _ => print("E")
          // print(g)
        println()
       
    def showGalExt() : Unit = 
      for (y <- List.range(0,galExtLimY))
        for ( x <- List.range(0,galExtLimX))
          galExt.filter(_.p == Pos(x,y)).head match 
            case Gal(TypeTile.Galaxy,_) => print("G")
            case Gal(TypeTile.Void,_) => print(".")
            case _ => print("E")
          // print(g)
        println()

    def createGal() : List[Gal] = 
      var ltmp : List[Gal] = List()
      for (y <- List.range(0,data.length))
        for (x <- List.range(0,data.head.length))
          val t : TypeTile = if (data(y)(x) == "#") then TypeTile.Galaxy else TypeTile.Void 
          // print(data(y)(x))
          ltmp ::= Gal(t,Pos(x,y))
        // println()
      return ltmp


    def getListFullVoid(lg : List[Gal]) : (List[Int],List[Int]) = 
      val limx = lg.foldLeft(0)((c,e) => if (c < e.p.x) then e.p.x else c) + 1
      val limy = lg.foldLeft(0)((c,e) => if (c < e.p.y) then e.p.y else c) + 1
      var lx : List[Int] = List()
      var ly : List[Int] = List()
      for (y <- List.range(0,limy))
        if (lg.filter(t => t.p.y == y && t.t ==TypeTile.Void).length == limy) {
          ly ::= y
        }
      for (x <- List.range(0,limx))
        if (lg.filter(t => t.p.x == x && t.t ==TypeTile.Void).length == limx) {
          lx ::= x
        }
      return (lx.sorted,ly.sorted)


    def createGalExt() : List[Gal] = 
      var galTmp : List[Gal] = gal
      var ly : List[Int] = List()
      var lx : List[Int] = List()
      var limy = galLimY
      var limx = galLimX
      // find line and column with full void
      for (y <- List.range(0,galLimY))
        if (galTmp.filter(t => t.p.y == y && t.t == TypeTile.Void).length == limy ) {
          ly ::= y
        }
      for (x <- List.range(0,galLimX))
        if (galTmp.filter(t => t.p.x == x && t.t == TypeTile.Void).length == limx ) {
          lx ::= x
        }
      
        // sorted the result
      lx = lx.sorted
      ly = ly.sorted
      // println("lx = [%s] - ly = [%s]".format(lx,ly))

      // move each line who are not full of void
      var cptx : Int = 0
      for (n <- lx) 
        galTmp = galTmp.map(g => if (g.p.x > n+cptx) then Gal(g.t,Pos(g.p.x+1,g.p.y)) else g) 
        cptx+=1

      var cpty : Int = 0
      for (n <- ly) 
        galTmp = galTmp.map(g => if (g.p.y > n+cpty) then Gal(g.t,Pos(g.p.x,g.p.y+1)) else g) 
        cpty+=1
     
      // Add new line and column with Void
      cptx = 0
      for (n <- lx) 
        galTmp :::= galTmp.filter(t => t.p.x == n+cptx).map(t => Gal(TypeTile.Void,Pos(n+cptx+1,t.p.y))) 
        cptx+=1

      cpty = 0
      for (n <- ly) 
        galTmp :::= galTmp.filter(t => t.p.y == n+cpty).map(t => Gal(TypeTile.Void,Pos(t.p.x,n+cpty+1))) 
        cpty+=1
      
      return galTmp

    @tailrec
    final def getPairsFromList[A](l:List[A],res:List[(A,A)]) : List[(A,A)] =
      if (l.length <= 1) {
        return res
      } else {
        return getPairsFromList(l.tail,res:::(l.tail.map((l.head,_)))) 
      }

    def diffBetweenPos(p1:Pos,p2:Pos) : Int =
      val x = if (p1.x < p2.x) then (p2.x - p1.x) else (p1.x - p2.x)
      val y = if (p1.y < p2.y) then (p2.y - p1.y) else (p1.y - p2.y)
      return x+y

    def getPathForAllGalaxiesPairs() : Int = 
      val listG = galExt.filter(t => t.t == TypeTile.Galaxy)
      val listP : List[(Gal,Gal)] = getPairsFromList(listG,List()) 
      val res = listP.map((g1,g2) => diffBetweenPos(g1.p,g2.p)).foldLeft(0)((c,e) => c+e)
      return res

    def diffBetweenGal(g1:Gal,g2:Gal,fill:Long,lx:List[Int],ly:List[Int]) : Long =
      val p1 = g1.p
      val p2 = g2.p
      val x : Long = if (p1.x < p2.x) {
        (p2.x - p1.x) + ((lx.filter(x => p1.x < x && p2.x > x).length)*(fill-1))
      } else {
        (p1.x - p2.x) + ((lx.filter(x => p2.x < x && p1.x > x).length)*(fill-1))
      }
      val y : Long = if (p1.y < p2.y) {
        (p2.y - p1.y) + ((ly.filter(y => p1.y < y && p2.y > y).length)*(fill-1))
      } else {
        (p1.y - p2.y) + ((ly.filter(y => p2.y < y && p1.y > y).length)*(fill-1))
      }
      
      return x+y

    def getPathForAllGalaxiesPairs(fill:Long) : Long =
      val listG = gal.filter(t => t.t == TypeTile.Galaxy)
      val (listFVx,listFVy) : (List[Int],List[Int]) = getListFullVoid(gal)
      val listP : List[(Gal,Gal)] = getPairsFromList(listG,List())
      val res = listP.map((g1,g2) => diffBetweenGal(g1,g2,fill,listFVx,listFVy)).foldLeft(0L)((c,e) => c+e)
      return res

  def runStep1(p: os.Path, f: String, debug:Boolean) : Int =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val charMap : List[List[String]] = data.fold[List[List[String]]](List())((c,e) => c:::List(e.toList.map(_+"")))
    val mapGal : MapGal = MapGal(charMap)
    if (debug) {
      mapGal.showGal()
      println()
      mapGal.showGalExt()
    }
    return mapGal.getPathForAllGalaxiesPairs()

  def runStep2(p: os.Path, f: String, debug:Boolean, fill:Long) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val charMap : List[List[String]] = data.fold[List[List[String]]](List())((c,e) => c:::List(e.toList.map(_+"")))
    val mapGal : MapGal = MapGal(charMap)
    if (debug) {
      mapGal.showGal()
      println()
      mapGal.showGalExt()
    }
    return mapGal.getPathForAllGalaxiesPairs(fill)


@main def runDay11() = 
  val day = Day11()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  println("Step2 : Sample")
  println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true,10L))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false,1000000L))
  
