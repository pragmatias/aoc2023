package fr.pragmatias.exos

import fr.pragmatias.tools.Utils
import scala.annotation.tailrec


class Day16() :

  val dataFileS1 = "day16_step1.txt"
  val dataFileFull = "day16.txt"


  enum Energy :
    case On, Off
  enum Dir :
    case U, D, L, R
    def opposed() : Dir = 
      return this match
        case Dir.U => Dir.D
        case Dir.D => Dir.U
        case Dir.L => Dir.R
        case Dir.R => Dir.L
      
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

    def isOnPlate(b:Beam):Boolean = (b.p.x >= 0 && b.p.x < limitX && b.p.y >= 0 && b.p.y < limitY)

    def getMove(d:Dir,p:Pos) : Pos =
      return d match 
            case Dir.U => Pos(p.x,p.y-1)
            case Dir.D => Pos(p.x,p.y+1)
            case Dir.L => Pos(p.x-1,p.y)
            case Dir.R => Pos(p.x+1,p.y)

    def moveBeam(b:Beam) : List[Beam] =
      val t = plateau(b.p.y)(b.p.x)
      t.obs match 
        case '.' => return List(Beam(b.d,getMove(b.d,b.p)))
        case '-' => {
          b.d match
            case Dir.U | Dir.D => return List(Beam(Dir.L,getMove(Dir.L,b.p)),Beam(Dir.R,getMove(Dir.R,b.p)))
            case Dir.L => return List(Beam(Dir.L,getMove(Dir.L,b.p)))
            case Dir.R => return List(Beam(Dir.R,getMove(Dir.R,b.p)))
        } 
        case '|' => {
          b.d match
            case Dir.L | Dir.R => return List(Beam(Dir.U,getMove(Dir.U,b.p)),Beam(Dir.D,getMove(Dir.D,b.p)))
            case Dir.U => return List(Beam(Dir.U,getMove(Dir.U,b.p)))
            case Dir.D => return List(Beam(Dir.D,getMove(Dir.D,b.p)))
        }
        case '/' => {
          b.d match
            case Dir.L => return List(Beam(Dir.D,getMove(Dir.D,b.p)))
            case Dir.R => return List(Beam(Dir.U,getMove(Dir.U,b.p)))
            case Dir.U => return List(Beam(Dir.R,getMove(Dir.R,b.p)))
            case Dir.D => return List(Beam(Dir.L,getMove(Dir.L,b.p)))
        }
        case '\\' => {
          b.d match
            case Dir.L => return List(Beam(Dir.U,getMove(Dir.U,b.p)))
            case Dir.R => return List(Beam(Dir.D,getMove(Dir.D,b.p)))
            case Dir.D => return List(Beam(Dir.R,getMove(Dir.R,b.p)))
            case Dir.U => return List(Beam(Dir.L,getMove(Dir.L,b.p)))
        }
        case _ => return List()

    @tailrec
    final def execEnergizing(lb:List[Beam],lres:List[Beam]) : List[Beam] =
      if (lb.isEmpty) then 
        return lres.distinct
      else 
        val nlres = (lres:::lb)
        val nlb = lb.flatMap(b => moveBeam(b)).filter(b => isOnPlate(b) && lres.filter(x => b.isCompatible(x)).length == 0).distinct
        return execEnergizing(nlb,nlres)
    
    def setEnergyOn(lp:List[Pos]) : Unit =
      for (p <- lp) do 
        plateau(p.y)(p.x) = Tile(plateau(p.y)(p.x).obs,Energy.On)


    def getMaxEnergy() : Int = 
      val lNorthSouth = (0 until limitX).foldLeft[List[Beam]](List())((c,e) => Beam(Dir.D,Pos(e,0))::Beam(Dir.U,Pos(e,limitY-1))::c)
      val lWestEast = (0 until limitY).foldLeft[List[Beam]](List())((c,e) => Beam(Dir.R,Pos(0,e))::Beam(Dir.L,Pos(limitX-1,e))::c)
      val res = (lNorthSouth:::lWestEast).map(b => execEnergizing(List(b),List()).map(_.p).distinct.length)
      return res.max

  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val pl = Plate(data.toList)
    val res = Utils.getTime("",debug,pl.execEnergizing(List(Beam(Dir.R,Pos(0,0))),List()).map(_.p).distinct)
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
  
