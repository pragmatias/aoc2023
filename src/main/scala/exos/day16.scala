package exos

import fr.pragmatias.tools.Utils
import scala.annotation.tailrec


class Day16() :

  val dataFileS1 = "day16_step1.txt"
  val dataFileFull = "day16.txt"


  enum Energy :
    case On, Off
  enum Dir :
    case Up,Down,Left,Right
    def opposed() : Dir = 
      return this match
        case Dir.Up => Dir.Down
        case Dir.Down => Dir.Up
        case Dir.Left => Dir.Right
        case Dir.Right => Dir.Left
      
  case class Tile(obs:Char,enr:Energy) :
    override def toString() : String = "[ %s ,%s]".format(obs,enr.toString().substring(0,2))
  case class Pos(x:Int,y:Int)
  case class Beam(d : Dir, p : Pos) :
    def isCompatible(b:Beam) : Boolean =
      return ((p.x == b.p.x && p.y == b.p.y) && (b.d == d))
      

  class Plate(data : List[String]) : 
    val plateau : Array[Array[Tile]] = data.map(_.toArray.map(c => Tile(c,Energy.Off))).toArray
    val limitX = plateau.head.length 
    val limitY = plateau.length
    override def toString() : String = plateau.map(l => l.mkString("")).mkString("\n")


    def getMove(d:Dir,p:Pos) : Pos =
      return d match 
            case Dir.Up => Pos(p.x,p.y-1)
            case Dir.Down => Pos(p.x,p.y+1)
            case Dir.Left => Pos(p.x-1,p.y)
            case Dir.Right => Pos(p.x+1,p.y)

    def moveBeam(b:Beam) : List[Beam] =
      val t = plateau(b.p.y)(b.p.x)
      t.obs match 
        case '.' => return List(Beam(b.d,getMove(b.d,b.p)))
        case '-' => {
          b.d match
            case Dir.Up | Dir.Down => return List(Beam(Dir.Left,getMove(Dir.Left,b.p)),Beam(Dir.Right,getMove(Dir.Right,b.p)))
            case Dir.Left => return List(Beam(Dir.Left,getMove(Dir.Left,b.p)))
            case Dir.Right => return List(Beam(Dir.Right,getMove(Dir.Right,b.p)))
        } 
        case '|' => {
          b.d match
            case Dir.Left | Dir.Right => return List(Beam(Dir.Up,getMove(Dir.Up,b.p)),Beam(Dir.Down,getMove(Dir.Down,b.p)))
            case Dir.Up => return List(Beam(Dir.Up,getMove(Dir.Up,b.p)))
            case Dir.Down => return List(Beam(Dir.Down,getMove(Dir.Down,b.p)))
        }
        case '/' => {
          b.d match
            case Dir.Left => return List(Beam(Dir.Down,getMove(Dir.Down,b.p)))
            case Dir.Right => return List(Beam(Dir.Up,getMove(Dir.Up,b.p)))
            case Dir.Up => return List(Beam(Dir.Right,getMove(Dir.Right,b.p)))
            case Dir.Down => return List(Beam(Dir.Left,getMove(Dir.Left,b.p)))
        }
        case '\\' => {
          b.d match
            case Dir.Left => return List(Beam(Dir.Up,getMove(Dir.Up,b.p)))
            case Dir.Right => return List(Beam(Dir.Down,getMove(Dir.Down,b.p)))
            case Dir.Down => return List(Beam(Dir.Right,getMove(Dir.Right,b.p)))
            case Dir.Up => return List(Beam(Dir.Left,getMove(Dir.Left,b.p)))
        }
        case _ => return List()

    @tailrec
    final def execEnergizing(lb:List[Beam],lres:List[Beam]) : List[Beam] =
      if (lb.isEmpty) then 
        return lres.distinct
      else 
        val nlres = (lres:::lb)
        val nlb = lb.flatMap(b => moveBeam(b)).filter(b => b.p.x >= 0 && b.p.x < limitX && b.p.y >= 0 && b.p.y < limitY && lres.filter(x => b.isCompatible(x)).length == 0).distinct
        return execEnergizing(nlb,nlres)
    
    def setEnergyOn(lp:List[Pos]) : Unit =
      for (p <- lp) do 
        plateau(p.y)(p.x) = Tile(plateau(p.y)(p.x).obs,Energy.On)


    def getMaxEnergy() : Int = 
      val lNorthSouth = (0 until limitX).foldLeft[List[Beam]](List())((c,e) => Beam(Dir.Down,Pos(e,0))::Beam(Dir.Up,Pos(e,limitY-1))::c)
      val lWestEast = (0 until limitY).foldLeft[List[Beam]](List())((c,e) => Beam(Dir.Right,Pos(0,e))::Beam(Dir.Left,Pos(limitX-1,e))::c)
      val res = (lNorthSouth:::lWestEast).map(b => execEnergizing(List(b),List()).map(_.p).distinct.length)
      return res.max

  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val pl = Plate(data.toList)
    val res = Utils.getTime("",debug,pl.execEnergizing(List(Beam(Dir.Right,Pos(0,0))),List()).map(_.p).distinct)
    if (debug) then
      println(pl)
      println(res.mkString("\n"))
    return res.length

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val pl = Plate(data.toList)
    return pl.getMaxEnergy()


@main def runDay16() = 
  val day = Day16()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
