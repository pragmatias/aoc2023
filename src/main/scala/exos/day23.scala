package fr.pragmatias.exos

import fr.pragmatias.tools.Utils
import scala.collection.immutable.HashMap
import scala.annotation.tailrec
import scala.collection.immutable.BitSet

class Day23() :

  val dataFileS1 = "day23_step1.txt"
  val dataFileFull = "day23.txt"

  enum Trail :
    case Path, SlopeUp, SlopeDown,SlopeLeft, SlopeRight

  case class Pos(x:Int, y:Int) :
    def getDist(p:Pos) : Int = Math.abs(x-p.x) + Math.abs(y-p.y)
  case class Tile(trail:Trail, pos:Pos) :
    def getDist(t:Tile) : Int = this.pos.getDist(t.pos)
  case class Junction(tStart : Tile, tEnd: Tile)


  case class Hiking(listTile : List[Tile]) :
    val limX = listTile.map(_.pos.x).max 
    val limY = listTile.map(_.pos.y).max 

    def getStart() : Tile = listTile.filter(t => t.pos.y == 0 && t.trail == Trail.Path).head
    def getEnd() : Tile = listTile.filter(t => t.pos.y == limY && t.trail == Trail.Path).head

    def nextMove(t : Tile) : List[Tile] = 
      return t match
      case Tile(Trail.Path,p) => List(Pos(p.x+1,p.y),Pos(p.x-1,p.y),Pos(p.x,p.y-1),Pos(p.x,p.y+1))
                                    .filter(p => listTile.filter(t => t.pos == p).length == 1)
                                    .map(p => listTile.filter(t => t.pos == p).head)
      case Tile(Trail.SlopeUp,p) => listTile.filter(t => t.pos == p.copy(y=p.y-1))
      case Tile(Trail.SlopeDown,p) => listTile.filter(t => t.pos == p.copy(y=p.y+1))
      case Tile(Trail.SlopeLeft,p) => listTile.filter(t => t.pos == p.copy(x=p.x-1))
      case Tile(Trail.SlopeRight,p) => listTile.filter(t => t.pos == p.copy(x=p.x+1))

    def searchAllJunction() : HashMap[Junction,Int] = 
      val listCross = getStart()::getEnd()::listTile.filter(t => nextMove(t).length > 2)
      val mapJunction : HashMap[Junction,Int] = HashMap.empty

      @tailrec
      def searchJunction(start:Tile,prec:Tile,last:Tile, dist : Int) : Option[(Junction,Int)] =
        if listCross.contains(last) then
          Some(Junction(start,last),dist)
        else 
          val nm = nextMove(last).filter(_ != prec)
          if nm.length != 1 then 
            // if start.pos==Pos(3,5) then println("start : %s - last : %s".format(start,last))
            None
          else 
            searchJunction(start,last,nm.head,dist+1)
          
      val res = listCross.flatMap(c => nextMove(c).map(t => (c,t))).foldLeft(mapJunction):
        case (chm,(start,next)) => searchJunction(start,start,next,1) match
          case Some(junc) => HashMap(junc)++chm
          case None => chm

      return res


    def searchLongestPath() : Int = 
      val hmJ = searchAllJunction()
      val start = getStart()
      val lStart = hmJ.filterKeys(j => j.tStart == start).toList
      val hmRes : scala.collection.mutable.HashMap[Tile,Int] = scala.collection.mutable.HashMap.empty 

      @tailrec
      def searchLongestPath(start:List[(Junction,Int)],lvisited : List[Junction]) : Int =
        // println("Appel search : ")
        // start.foreach(println)
        if (start.isEmpty) then
          return hmRes(getEnd()) //get the end tile distance
        else 
          val lj = start.flatMap(x => hmJ.filterKeys(j => j.tStart == x._1.tEnd && j.tEnd != x._1.tStart)
                                                           // && !lvisited.contains(j) && !lvisited.contains(Junction(j.tEnd,j.tStart)))
                                          .map(y => (y._1,y._2+x._2)))
          for i <- lj do 
            val tmp = hmRes.get(i._1.tEnd)
            tmp match
              case Some(num) => if i._2 > num then hmRes(i._1.tEnd) = i._2
              case None => hmRes(i._1.tEnd) = i._2
             
          return searchLongestPath(lj,start.map(_._1):::lvisited)
        
      return searchLongestPath(lStart,List())
        

    def searchLongestPathP2() : Int = 
      val hmJ = searchAllJunction()
      val allCrossWithIndex = (hmJ.map(_._1.tEnd).toList:::hmJ.map(_._1.tStart).toList).toList.distinct.sortBy(_.getDist(getStart())).zipWithIndex
      val mapIndex : Map[Int,Tile] = allCrossWithIndex.foldLeft(Map.empty):
        case (m,(e,i)) => Map((i->e))++m
      val mapTile : Map[Tile,Int] = allCrossWithIndex.foldLeft(Map.empty):
        case (m,(e,i)) => Map((e->i))++m
      val nextCross : Map[Int,List[(Int,Int)]] = mapIndex.foldLeft(Map.empty):
        case (m,(i,e)) => Map((i -> hmJ.filterKeys(j => j.tStart == e).map(x => (mapTile(x._1.tEnd),x._2)).toList))++m

        
      val start = mapTile(getStart())
 
      def searchLongestPathP2(junc:Int,lvisited : BitSet,dist:Int) : Int =
        // println("who am i : %s - %s".format(junc,mapIndex(junc)))
        if (mapIndex(junc) == getEnd()) then
          // println("enddd ?")
          return dist 
        else 
          val nJunc = nextCross(junc).filter(j => !lvisited(j._1))
          // println("test : "+nJunc)
                                          
          return nJunc.foldLeft(0):
            case (distCur,(nj,dj)) =>  
                distCur.max(searchLongestPathP2(nj,lvisited + nj,dj+dist))
             
      return searchLongestPathP2(start,BitSet.empty,0)
        






  def extractTileFromInput(data : List[String]) : List[Tile] =
    val dataC = data.map(_.toList)
    val limX = dataC.head.length
    val limY = dataC.length
    val res = (0 until limY).foldLeft[List[Tile]](List()):
                case (lres,y) => ((0 until limX).foldLeft[List[Tile]](List()):
                   case(l,x) =>  (dataC(y)(x) match 
                                  case '.' => List(Tile(Trail.Path,Pos(x,y)))
                                  case '^' => List(Tile(Trail.SlopeUp,Pos(x,y)))
                                  case 'v' => List(Tile(Trail.SlopeDown,Pos(x,y)))
                                  case '>' => List(Tile(Trail.SlopeRight,Pos(x,y)))
                                  case '<' => List(Tile(Trail.SlopeLeft,Pos(x,y)))
                                  case _ => List()) ::: l ) ::: lres
    return res
  
  def extractTileFromInputP2(data : List[String]) : List[Tile] =
    val dataC = data.map(_.toList)
    val limX = dataC.head.length
    val limY = dataC.length
    val res = (0 until limY).foldLeft[List[Tile]](List()):
                case (lres,y) => ((0 until limX).foldLeft[List[Tile]](List()):
                   case(l,x) =>  (dataC(y)(x) match 
                                  case '.' | '^' | 'v' | '>' | '<' => List(Tile(Trail.Path,Pos(x,y)))
                                  case _ => List()) ::: l ) ::: lres
    return res

  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val lTile : List[Tile] = extractTileFromInput(data.toList)
    val hiking : Hiking = Hiking(lTile)
    val junctions : HashMap[Junction,Int] = hiking.searchAllJunction()
    val res = hiking.searchLongestPath()
    if debug then 
      // lTile.foreach(println)
      junctions.foreach(println)
    return res

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val lTile : List[Tile] = extractTileFromInputP2(data.toList)
    val hiking : Hiking = Hiking(lTile)
    val junctions : HashMap[Junction,Int] = hiking.searchAllJunction()
    val res = hiking.searchLongestPathP2()
    if debug then 
      // lTile.foreach(println)
      junctions.foreach(println)
    return res


@main def runDay23() = 
  val day = Day23()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,false))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  println("Step2 : Sample")
  println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,false))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
