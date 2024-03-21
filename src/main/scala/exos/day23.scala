package fr.pragmatias.exos

import fr.pragmatias.tools.Utils
import scala.collection.immutable.HashMap
import scala.annotation.tailrec
import scala.collection.immutable.BitSet

class Day23() :

  val dataFileS1 = "day23_step1.txt"
  val dataFileFull = "day23.txt"

  enum Trail :
    case P, SU, SD,SL, SR, Wall

  enum Dir:
    case N, E, W, S

  case class Pos(x:Int, y:Int) :
    def getDist(p:Pos) : Int = Math.abs(x-p.x) + Math.abs(y-p.y)
  case class PosWithDir(pos:Pos,d:Dir):
    def getDist(p:PosWithDir) : Int = this.pos.getDist(p.pos)
    def getListPosWithDir(t:Trail) : List[PosWithDir] = 
      t match 
      case Trail.SU => List(this.copy(pos=pos.copy(y=pos.y-1)))
      case Trail.SD => List(this.copy(pos=pos.copy(y=pos.y+1)))
      case Trail.SL => List(this.copy(pos=pos.copy(x=pos.x-1)))
      case Trail.SR => List(this.copy(pos=pos.copy(x=pos.x+1)))
      case Trail.P => List(this.copy(pos=pos.copy(y=pos.y-1),d=Dir.N)
        ,this.copy(pos=pos.copy(y=pos.y+1),d=Dir.S)
        ,this.copy(pos=pos.copy(x=pos.x+1),d=Dir.E)
        ,this.copy(pos=pos.copy(x=pos.x-1),d=Dir.W))
        // (if (d!=Dir.S) then List(this.copy(pos=pos.copy(y=pos.y-1),d=Dir.N)) else List())
        // ::: (if (d!=Dir.N) then List(this.copy(pos=pos.copy(y=pos.y+1),d=Dir.S)) else List())
        // ::: (if (d!=Dir.W) then List(this.copy(pos=pos.copy(x=pos.x+1),d=Dir.E)) else List())
        // ::: (if (d!=Dir.E) then List(this.copy(pos=pos.copy(x=pos.x-1),d=Dir.W)) else List())
      case _ => List()



  case class Tile(trail:Trail, pos : PosWithDir) :
    def getDist(t:Tile) : Int = this.pos.getDist(t.pos)
  case class Junction(tStart : Tile, tEnd: Tile)


  case class Hiking(map : Array[Array[Trail]]) :
    val limX = map.head.length 
    val limY = map.length 
    def isInH(p:Pos) : Boolean = p.x >= 0 && p.y >= 0 && p.x < limX && p.y < limY
    def isWalkable(pos:PosWithDir) : Boolean = 
      val p = pos.pos
      isInH(p) && map(p.y)(p.x) != Trail.Wall 
      && ( pos.d match 
        case Dir.N => map(p.y)(p.x) != Trail.SD
        case Dir.S => map(p.y)(p.x) != Trail.SU 
        case Dir.E => map(p.y)(p.x) != Trail.SL 
        case Dir.W => map(p.y)(p.x) != Trail.SR )
    def isCross(p:Pos) : Boolean = 
      val t = map(p.y)(p.x)
      val list = (if t != Trail.Wall then List(p.copy(x=p.x-1),p.copy(x=p.x+1),p.copy(y=p.y-1),p.copy(y=p.y+1)) else List())
      return list.filter(p => isInH(p) && map(p.y)(p.x) != Trail.Wall).length > 2
    def getTile(p:PosWithDir) : Tile = Tile(map(p.pos.y)(p.pos.x),p)
    def getAllCrossPos() : List[Pos] = 
      val lp = 
        for 
          y <- map.indices 
          x <- map.head.indices
        yield Pos(x,y)
      lp.filter(isCross).toList

    def getStart() : Pos = Pos(map.head.indexWhere(_ == Trail.P),0)
    def getEnd() : Pos = Pos(map(limY-1).indexWhere(_ == Trail.P),limY-1)

    def nextMove(t : Tile) : List[Tile] = 
      t match 
      case Tile(t,p) => p.getListPosWithDir(t).filter(isWalkable).map(getTile)

    def searchAllJunction() : HashMap[Junction,Int] = 
      val listCross :List[Pos] = getStart()::getEnd()::getAllCrossPos()
      val mapJunction : HashMap[Junction,Int] = HashMap.empty

      @tailrec
      def searchJunction(start:Tile,prec:Tile,last:Tile, dist : Int) : Option[(Junction,Int)] =
        if listCross.contains(last.pos.pos) then
          Some(Junction(start,last),dist)
        else 
          val nm = nextMove(last).filter(_.pos.pos != prec.pos.pos)
          if nm.length != 1 then 
            // if start.pos==Pos(3,5) then println("start : %s - last : %s".format(start,last))
            None
          else 
            searchJunction(start,last,nm.head,dist+1)
          
      val res = listCross.map(c => getTile(PosWithDir(c,Dir.N))).flatMap(tc => nextMove(tc).map(t => (tc.copy(pos=tc.pos.copy(d=t.pos.d)),t)))
        .foldLeft(mapJunction):
        case (chm,(start,next)) => searchJunction(start,start,next,1) match
          case Some(junc) => HashMap(junc)++chm
          case None => chm

      return res


    def searchLongestPath() : Int = 
      val hmJ = searchAllJunction()
      val start = getStart()
      val lStart = hmJ.filterKeys(j => j.tStart.pos.pos == start).toList
      val hmRes : scala.collection.mutable.HashMap[Pos,Int] = scala.collection.mutable.HashMap.empty 

      @tailrec
      def searchLongestPath(start:List[(Junction,Int)],lvisited : List[Junction]) : Int =
        // println("Appel search : ")
        // start.foreach(println)
        if (start.isEmpty) then
          return hmRes(getEnd()) //get the end tile distance
        else 
          val lj = start.flatMap(x => hmJ.filterKeys(j => j.tStart.pos.pos == x._1.tEnd.pos.pos && j.tEnd.pos.pos != x._1.tStart.pos.pos)
                                                           // && !lvisited.contains(j) && !lvisited.contains(Junction(j.tEnd,j.tStart)))
                                          .map(y => (y._1,y._2+x._2)))
          for i <- lj do 
            val tmp = hmRes.get(i._1.tEnd.pos.pos)
            tmp match
              case Some(num) => if i._2 > num then hmRes(i._1.tEnd.pos.pos) = i._2
              case None => hmRes(i._1.tEnd.pos.pos) = i._2
             
          return searchLongestPath(lj,start.map(_._1):::lvisited)
        
      return searchLongestPath(lStart,List())
        




    def searchLongestPathP2() : Int = 
      val hmJ = searchAllJunction()
      val allCrossWithIndex = (hmJ.map(_._1.tEnd).toList:::hmJ.map(_._1.tStart).toList).toList.distinct.sortBy(_.pos.pos.getDist(getStart())).zipWithIndex
      val mapIndex : Map[Int,Tile] = allCrossWithIndex.foldLeft(Map.empty):
        case (m,(e,i)) => Map((i->e))++m
      val mapTile : Map[Tile,Int] = allCrossWithIndex.foldLeft(Map.empty):
        case (m,(e,i)) => Map((e->i))++m
      val nextCross : Map[Int,List[(Int,Int)]] = mapIndex.foldLeft(Map.empty):
        case (m,(i,e)) => Map((i -> hmJ.filterKeys(j => j.tStart == e).map(x => (mapTile(x._1.tEnd),x._2)).toList))++m

        
      val start = mapTile(getTile(PosWithDir(getStart(),Dir.S)))
 
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
        






  def extractTileFromInput(data : List[String]) : Array[Array[Trail]] =
    data.map(_.toList.map(c => 
        c match 
        case '.' => Trail.P
        case '^' => Trail.SU
        case 'v' => Trail.SD 
        case '>' => Trail.SR 
        case '<' => Trail.SL
        case _   => Trail.Wall
        ).toArray).toArray
  
  def extractTileFromInputP2(data : List[String]) : Array[Array[Trail]] =
    data.map(_.toList.map(c => 
        c match 
        case '.' | '^' | 'v' | '>' | '<' => Trail.P
        case _ => Trail.Wall
        ).toArray).toArray

  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val hiking : Hiking = Hiking(extractTileFromInput(data.toList))
    val res = Utils.getTime(debug,hiking.searchLongestPath())
    return res

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val hiking : Hiking = Hiking(extractTileFromInputP2(data.toList))
    val res = Utils.getTime(debug,hiking.searchLongestPathP2())
    return res


@main def runDay23() = 
  val day = Day23()
  println("Step1 : Sample")
  println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  println("Step1 : Full")
  println(day.runStep1(Utils.dataFullPath,day.dataFileFull,true))
  println("Step2 : Sample")
  println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step2 : Full")
  // println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
