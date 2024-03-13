package fr.pragmatias.exos

import fr.pragmatias.tools.Utils
import scala.annotation.tailrec

class Day21() :

  val dataFileS1 = "day21_step1.txt"
  val dataFileFull = "day21.txt"

  case class Pos(x:Int,y:Int) : 
    def getNextPos() : List[Pos] = 
      return List(Pos(x+1,y),Pos(x-1,y),Pos(x,y+1),Pos(x,y-1))
  
  case class Garden(startPos:Pos,lenX:Int, lenY:Int, listRocks:List[Pos]) :
    def getNextPos(pos : Pos) : List[Pos] =
      return pos.getNextPos().filter(p => p.x >= 0 && p.y >= 0 && p.x < lenX && p.y < lenY && !listRocks.contains(p))
    

  def extractGarden(data : List[String]) : Garden =
    val dataGarden = data.map(s => s.toList).toList
    var startPos = Pos(0,0)
    val listRocks = dataGarden.indices.foldLeft[List[Pos]](List()):
      case (l,y) => (dataGarden(y).indices.foldLeft[List[Pos]](List()):
        case (c,x) => dataGarden(y)(x) match
          case '#' => Pos(x,y)::c
          case 'S' => { startPos = Pos(x,y) ; c}
          case _ => c
          ) ::: l
    return Garden(startPos,dataGarden.head.length,dataGarden.length,listRocks)

  @tailrec
  final def execStep(nbrSteps:Int,listCurrPos:List[Pos],garden:Garden) : List[Pos] = 
    if nbrSteps <= 0 then listCurrPos
    else 
      val nlistCurrPos = listCurrPos.flatMap(garden.getNextPos).distinct
      execStep(nbrSteps-1,nlistCurrPos,garden)

  def runStep1(p: os.Path, f: String, debug:Boolean, nbrSteps:Int) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val garden = extractGarden(data.toList)
    val res = execStep(nbrSteps,List(garden.startPos),garden) 
    if debug then 
      println(garden)
    return res.length

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    return 0


@main def runDay21() = 
  val day = Day21()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true,6))
  println("Step1 : Full")
  println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false,64))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step2 : Full")
  // println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
