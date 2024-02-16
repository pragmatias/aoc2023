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

  class MapLoop(listChar : List[List[String]]) :
    val limitx : Int = listChar.head.length
    val limity : Int = listChar.length
    
    def showMap() : Unit =
      for( y <- List.range(0,limity)) 
        for( x <- List.range(0,limitx))
          print("["+listChar(y)(x)+"]")
        println("")

    val listValUp : List[String] = List("|","7","F" )
    val listValDown : List[String] = List("|","L","J")
    val listValLeft : List[String] = List("-","L","F" )
    val listValRight : List[String] = List("-","J","7")

    val listAngle : List[String] = List("7","F","L","J")
    val listNotAngle : List[String] = List("-","|")
    
    def getNewPos(p:Pos,n:Dir) : List[Pos] =
      var np : Pos = p
      var listTest : List[String] = List()
      n match 
        case Dir.Up => {
          np = Pos(p.x,p.y-1)
          listTest = listValUp
        }
        case Dir.Down => {
          np = Pos(p.x,p.y+1)
          listTest = listValDown
        }
        case Dir.Left => {
          np = Pos(p.x-1,p.y)
          listTest = listValLeft
        }
        case Dir.Right => {
          np = Pos(p.x+1,p.y)
          listTest = listValRight
        }
      if (checkPossiblePos(np,List()) && listTest.contains(listChar(np.y)(np.x))) {
        return List(np)
      } else {
        return List()
      }

    def possibleNextPos(v:String,p:Pos) : List[Pos] =
      return v match 
                case "|" => getNewPos(p,Dir.Up) ::: getNewPos(p,Dir.Down)
                case "-" => getNewPos(p,Dir.Left) ::: getNewPos(p,Dir.Right)
                case "L" => getNewPos(p,Dir.Up) ::: getNewPos(p,Dir.Right)
                case "J" => getNewPos(p,Dir.Up) ::: getNewPos(p,Dir.Left)
                case "7" => getNewPos(p,Dir.Down) ::: getNewPos(p,Dir.Left)
                case "F" => getNewPos(p,Dir.Down) ::: getNewPos(p,Dir.Right)
                case "S" => getNewPos(p,Dir.Up) ::: getNewPos(p,Dir.Down) ::: getNewPos(p,Dir.Left) ::: getNewPos(p,Dir.Right)
                case _ => List()
   
    def searchStartPos() : Pos =
      var p : Pos = Pos(0,0)
      Breaks.breakable { 
        for (y <- List.range(0,limity))
          for (x <- List.range(0,limitx))
            if (listChar(y)(x) == "S") {
              p = Pos(x,y)
              Breaks.break
            }
      }
      return p

    def checkPossiblePos(p:Pos,lbp:List[Pos]) : Boolean = 
      return !(lbp.contains(p) || p.x >= limitx || p.x < 0 || p.y >= limity || p.y < 0 || listChar(p.y)(p.x) == "." )


    @tailrec
    final def browseAllPaths(ls : List[Pos], lbp:List[Pos], step:Int) : Int =
      val nlbp = (lbp:::ls).toSeq
      val lnp : List[Pos] = ls.flatMap(p => possibleNextPos(listChar(p.y)(p.x),p).filter(checkPossiblePos(_,nlbp))).toSeq
      // println("step : %d - Pos : %s - OldPos : %s".format(step,lnp,nlbp))
      if (lnp.isEmpty) {
        return step
      } else {
        browseAllPaths(lnp,nlbp,step+1)
      }

    def browseAllPaths() : Int =
      val start : Pos = searchStartPos()
      return browseAllPaths(List(start),List(),0)

    @tailrec
    final def findAngle(p:Pos, lbp:List[Pos], la : List[(Int,Pos)], step:Int) : List[(Int,Pos)] = 
      // println("step : %s - p : %s - lbp : %s - la : %s".format(step,p,lbp,la))
      val nlbp = p::lbp
      val lnp : List[Pos] = possibleNextPos(listChar(p.y)(p.x),p).filter(checkPossiblePos(_,nlbp))
      if (lnp.isEmpty) {
        return la
      } else {
        val np = lnp.head 
        if (listAngle.contains(listChar(np.y)(np.x))) {
          findAngle(np,nlbp,la:::List((step,np)),step+1)
        } else {
          findAngle(np,nlbp,la,step)
        }
      }

    def startIsAngle(p:Pos) : Boolean = 
      val lnp : List[Pos] = possibleNextPos(listChar(p.y)(p.x),p).filter(checkPossiblePos(_,List()))
      val p1 = lnp(0)
      val p2 = lnp(1)
      return !(((p1.y == p2.y) && (p1.x != p2.x)) || ((p1.y != p2.y) && (p1.x == p2.x)))

    def calculAreaFromAngle(la : List[(Int,Pos)]) : Int = 
      var lres : List[Pos] = List()
      val laTmp = la.map(_._2)
      val nl = laTmp.zip(laTmp.tail:::List(laTmp.head)) 
      // println(nl)
      for (y <- List.range(0,limity))
        for (x <- List.range(0,limitx))
          if ((laTmp.filter(_ == Pos(x,y)).length == 0) && pointIsInPath(Pos(x,y),nl)) {
            lres ::= Pos(x,y)
            // println("Char : %s ".format(listChar(y)(x)))
          }
      // println("list(%d) In : %s".format(lres.length,lres))
      return lres.length

    def pointIsInPath(p:Pos,nl:List[(Pos,Pos)]) : Boolean = 
      var res = false
      Breaks.breakable { 
        for( i <- List.range(0,nl.length))
          val (p1,p2) : (Pos,Pos) = nl(i)
          if ((p.x == p1.x) && (p.y == p1.y)) {
            res = false
            Breaks.break
          } 
          if (((p.x >= p1.x && p.x <= p2.x) || (p.x <= p1.x && p.x >= p2.x)) && (p.y == p1.y)  && (p.y == p2.y)) {
            res = false
            Breaks.break
          }
          if (((p.y >= p1.y && p.y <= p2.y) || (p.y <= p1.y && p.y >= p2.y)) && (p.x == p1.x)  && (p.x == p2.x)) {
            res = false
            Breaks.break
          }
          if ((p1.y > p.y) != (p2.y > p.y)) {
            val slope = (p.x - p1.x) * ( p2.y - p1.y) - ( p2.x - p1.x ) * (p.y - p1.y)
            if (slope == 0) {
              res = true
              Breaks.break
            }
            if ((slope < 0) != (p2.y < p1.y)) {
              res = ! res
            }
          }
      }
      return res

    def calculArea() : Int = 
      val start : Pos = searchStartPos()
      // check if start is an angle or not 
      val startAngle : List[(Int,Pos)] = if (startIsAngle(start)) then List((0,start)) else List()
      val res = calculAreaFromAngle(findAngle(start,List(),startAngle,1))
      // println("Angle : "+res.map((z,x) => (z,listChar(x.y)(x.x))))
      return res


  def runStep1(p: os.Path, f: String, debug:Boolean) : Int =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val charMap : List[List[String]] = data.fold[List[List[String]]](List())((c,e) => c:::List(e.toList.map(_+"")))
    val context = MapLoop(charMap)
    return context.browseAllPaths()

  def runStep2(p: os.Path, f: String, debug:Boolean) : Int = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val charMap : List[List[String]] = data.fold[List[List[String]]](List())((c,e) => c:::List(e.toList.map(_+"")))
    val context = MapLoop(charMap)
    return context.calculArea()


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
  // println("Step2 : Sample - step2-3")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS23,true))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
