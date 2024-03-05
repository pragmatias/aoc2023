package exos

import fr.pragmatias.tools.Utils

class Day23() :

  val dataFileS1 = "day23_step1.txt"
  val dataFileFull = "day23.txt"

  enum Trail :
    case Path, SlopeUp, SlopeDown,SlopeLeft, SlopeRight

  case class Pos(x:Int, y:Int)
  case class Tile(trail:Trail, pos:Pos)


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

   def findPath()


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
                                  case '>' => List(Tile(Trail.SlopeLeft,Pos(x,y)))
                                  case '<' => List(Tile(Trail.SlopeRight,Pos(x,y)))
                                  case _ => List()) ::: l ) ::: lres
    return res

  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val lTile : List[Tile] = extractTileFromInput(data.toList)
    if debug then 
      lTile.foreach(println)
    return 0

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    return 0


@main def runDay23() = 
  val day = Day23()
  println("Step1 : Sample")
  println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step2 : Full")
  // println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
