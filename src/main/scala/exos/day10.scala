package exos

import fr.pragmatias.tools.Utils
import scala.collection.immutable.HashMap
import scala.annotation.tailrec
import scala.util.control.Breaks

class Day10() :

  val dataFileS1 = "day10_step1.txt"
  val dataFileS21 = "day10_step2_1.txt"
  val dataFileS22 = "day10_step2_2.txt"
  val dataFileS23 = "day10_step2_3.txt"
  val dataFileFull = "day10.txt"

  case class Tile(v:String,p:Pos)
  case class Pos(x:Int,y:Int) 
  enum Dir :
    case Up, Down, Left, Right

  class MapLoop(listChar : List[List[Char]]) :
    val limitx : Int = listChar.head.length
    val limity : Int = listChar.length
    
    def showMap() : Unit =
      for( y <- List.range(0,limity)) 
        for( x <- List.range(0,limitx))
          print("["+listChar(y)(x)+"]")
        println("")

    def getNewPos(p:Pos,n:Dir) : Pos =
      n match 
        case Dir.Up    => Pos(p.x,p.y-1)
        case Dir.Down  => Pos(p.x,p.y+1)
        case Dir.Left  => Pos(p.x-1,p.y)
        case Dir.Right => Pos(p.x+1,p.y)

    def getConnectedNextPos(v:Char,p:Pos) : List[Pos] =
      return (v match 
                case '|' => List(getNewPos(p,Dir.Up),getNewPos(p,Dir.Down))
                case '-' => List(getNewPos(p,Dir.Left),getNewPos(p,Dir.Right))
                case 'L' => List(getNewPos(p,Dir.Up),getNewPos(p,Dir.Right))
                case 'J' => List(getNewPos(p,Dir.Up),getNewPos(p,Dir.Left))
                case '7' => List(getNewPos(p,Dir.Down),getNewPos(p,Dir.Left))
                case 'F' => List(getNewPos(p,Dir.Down),getNewPos(p,Dir.Right))
                case 'S' => List(getNewPos(p,Dir.Up),getNewPos(p,Dir.Down),getNewPos(p,Dir.Left),getNewPos(p,Dir.Right))
                            .filter(p1 => p1.x < limitx && p1.y < limity && p1.x >= 0 && p1.y >= 0)
                            .filter(p1 => getConnectedNextPos(listChar(p1.y)(p1.x),p1).contains(p))
                case _ => List())
   
    def searchStartPos() : Pos =
      var p : Pos = Pos(0,0)
      Breaks.breakable { 
        for (y <- List.range(0,limity))
          for (x <- List.range(0,limitx))
            if (listChar(y)(x) == 'S') {
              p = Pos(x,y)
              Breaks.break
            }
      }
      return p


    @tailrec
    final def browseAllPaths(ls : List[Pos], lbp:List[Pos], step:Int) : (Int,List[Pos]) =
      val nlbp = (lbp:::ls).toSeq
      val lnp : List[Pos] = ls.flatMap(p => getConnectedNextPos(listChar(p.y)(p.x),p)).filter(!nlbp.contains(_)).toSeq
      // println("step : %d - Pos : %s - OldPos : %s".format(step,lnp,nlbp))
      if (lnp.isEmpty) {
        return (step,nlbp)
      } else {
        return browseAllPaths(lnp,nlbp,step+1)
      }

    def browseAllPaths() : (Int,List[Pos]) =
      val start : Pos = searchStartPos()
      return browseAllPaths(List(start),List(),0)



    def connectToNorth(p:Pos) : Boolean =
      getConnectedNextPos(listChar(p.y)(p.x),p).contains(Pos(p.x,p.y-1))

    def enclosedInLine(y:Int,path:List[Pos]) : Int =
      // println("test : [%s]".format(listChar(y)))
      val (_, count) = listChar(y).indices.foldLeft((false,0))((c,e) => {
        (c,e) match 
        case ((enclosed,count),x) if path.contains(Pos(x,y)) => 
          (enclosed ^ connectToNorth(Pos(x,y)),count)
        case ((true,count),_) => (true,count+1)
        case ((false,count),_) => (false,count)
      }) 
      return count


    def calculArea() : Int = 
      val path = browseAllPaths()._2
      return listChar.indices.map(enclosedInLine(_,path)).sum


  def runStep1(p: os.Path, f: String, debug:Boolean) : Int =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val charMap : List[List[Char]] = data.fold[List[List[Char]]](List())((c,e) => c:::List(e.toList))
    val context = MapLoop(charMap)
    return (context.browseAllPaths()._1)

  def runStep2(p: os.Path, f: String, debug:Boolean) : Int = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val charMap : List[List[Char]] = data.fold[List[List[Char]]](List())((c,e) => c:::List(e.toList))
    val context = MapLoop(charMap)
    return Utils.getTime(charMap,debug,context.calculArea())


@main def runDay10() = 
  val day = Day10()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  // println("Step2 : Sample - step1")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step2 : Sample - step2-1")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS21,true))
  // println("Step2 : Sample - step2-2")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS22,true))
  println("Step2 : Sample - step2-3")
  println(day.runStep2(Utils.dataSamplePath,day.dataFileS23,true))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
