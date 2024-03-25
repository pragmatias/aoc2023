package fr.pragmatias.exos

import fr.pragmatias.tools.Utils
import scala.annotation.tailrec

class Day21() :

  val dataFileS1 = "day21_step1.txt"
  val dataFileFull = "day21.txt"

  case class Pos(x:Int,y:Int) : 
    def getNextPos() : Set[Pos] = 
      return Set(this.copy(x=x+1)
        ,this.copy(x=x-1)
        ,this.copy(y=y+1)
        ,this.copy(y=y-1))
   
  
  case class Garden(startPos:Pos,lenX:Int, lenY:Int, listRocks:Set[Pos]) :
    def isInGarden(pos:Pos) : Boolean = 
      pos.x >= 0 && pos.y >= 0 && pos.x < lenX && pos.y < lenY
    def getNextPos(pos : Pos) : Set[Pos] =
      return pos.getNextPos().filter(p => isInGarden(p) && !listRocks.contains(p))
    

  def extractGarden(data : List[String]) : Garden =
    val dataGarden = data.map(s => s.toList).toList
    var startPos = Pos(0,0)
    val listRocks = dataGarden.indices.foldLeft[List[Pos]](List()):
      case (l,y) => (dataGarden(y).indices.foldLeft[List[Pos]](List()):
        case (c,x) => dataGarden(y)(x) match
          case '#' => Pos(x,y) :: c
          case 'S' => { startPos = Pos(x,y) ; c}
          case _ => c
          ) ::: l
    return Garden(startPos,dataGarden.head.length,dataGarden.length,listRocks.toSet)

  @tailrec
  final def execStep(nbrSteps:Int,listCurrPos:Set[Pos],garden:Garden) : Long = 
    if nbrSteps <= 0 then listCurrPos.size
    else 
      val nlistCurrPos = listCurrPos.flatMap(garden.getNextPos)
      execStep(nbrSteps-1,nlistCurrPos.toSet,garden)
  @tailrec
  final def execStep2(nbrSteps:Int,listCurrPos:Set[Pos],garden:Garden,nbrStepsTotal:Int) : Long = 
    if nbrSteps <= 0 then listCurrPos.size
    else 
      val nlistCurrPos = listCurrPos.flatMap(garden.getNextPos).toSet
      println("Step : %S - Res : %s".format(nbrStepsTotal - (nbrSteps-1),nlistCurrPos.size))
      execStep2(nbrSteps-1,nlistCurrPos,garden,nbrStepsTotal)

  def runStep1(p: os.Path, f: String, debug:Boolean, nbrSteps:Int) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val garden = extractGarden(data.toList)
    val res = execStep(nbrSteps,Set(garden.startPos),garden)
    if debug then 
      println(garden)
    return res

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val garden = extractGarden(data.toList)
    val res = execStep2(256,Set(garden.startPos),garden,256)
    return 0


// Notes :
// Step 129 --> res : 7574
// Step 130 --> res : 7612
// Step 131 --> res : 7574
// Step 132 --> res : 7612
// Step 133 --> res : 7574
// Using Quadratic Formula : a*x² + b*x + c 


@main def runDay21() = 
  val day = Day21()
  // println("Step1 : Sample")
  // println(Utils.getTime(true,day.runStep1(Utils.dataSamplePath,day.dataFileS1,true,6)))
  // println("Step1 : Full - 64")
  // println(Utils.getTime(true,day.runStep1(Utils.dataFullPath,day.dataFileFull,false,64)))
  // println("Step1 : Full - 131")
  // println(Utils.getTime(true,day.runStep1(Utils.dataFullPath,day.dataFileFull,false,131)))
  // println("Step2 : Sample")
  // println(Utils.getTime(true,day.runStep2(Utils.dataSamplePath,day.dataFileS1,true)))
  // println("Step2 : Full")
  // println(Utils.getTime(true,day.runStep2(Utils.dataFullPath,day.dataFileFull,false)))
  
