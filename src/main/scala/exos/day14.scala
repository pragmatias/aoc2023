package exos

import fr.pragmatias.tools.Utils
import scala.collection.mutable.{Queue,ListBuffer}
import scala.annotation.tailrec

class Day14() :

  val dataFileS1 = "day14_step1.txt"
  val dataFileFull = "day14.txt"

  class Plate(data : List[List[Char]]) :
    var plateau : Array[Array[Char]] = data.map(_.toArray).toArray
    val limitX = plateau.head.length
    val limitY = plateau.length
    override def toString() : String = 
      val sb = StringBuilder("")
      for (i <- (0 until limitX).toIterator) do
        for (j <- (0 until limitY).toIterator) do
          sb.append(plateau(i)(j))
        sb.append('\n')
      return sb.toString

    def moveToNorth() : Unit = 
      moveToNS(List.range(0,limitY),List.range(0,limitX))
    def moveToSouth() : Unit =
      moveToNS(List.range(0,limitY),List.range(0,limitX).reverse)
    def moveToWest() : Unit = 
      moveToWE(List.range(0,limitY),List.range(0,limitX))
    def moveToEast() : Unit = 
      moveToWE(List.range(0,limitY),List.range(0,limitX).reverse)

    def moveToNS(lj :List[Int], li : List[Int]) : Unit =
      for (j <- lj) 
        var emptyQ : Queue[Int] = Queue[Int]()
        for (i <- li)
          plateau(i)(j) match 
            case '.' => emptyQ += i
            case '#' => emptyQ.clear()
            case 'O' => {
              if (!emptyQ.isEmpty) {
                val newf = emptyQ.dequeue()
                plateau(newf)(j) = plateau(i)(j)
                plateau(i)(j) = '.'
                emptyQ += i
              }
            }
    def moveToWE(lj :List[Int], li : List[Int]) : Unit =
      for (j <- lj) 
        var emptyQ : Queue[Int] = Queue[Int]()
        for (i <- li)
          plateau(j)(i) match 
            case '.' => emptyQ += i
            case '#' => emptyQ.clear()
            case 'O' => {
              if (!emptyQ.isEmpty) {
                val newf = emptyQ.dequeue()
                plateau(j)(newf) = plateau(j)(i)
                plateau(j)(i) = '.'
                emptyQ += i
              }
            }
    
    def getValorisation() : Long =
      plateau.foldLeft((limitY,0L))((c,e) => (c._1 - 1, c._2 + (c._1 * e.filter(_ == 'O').length)))._2

    def rotateCycle() : Unit =
      moveToNorth()
      moveToWest()
      moveToSouth()
      moveToEast()


    def execRotation(n : Long) : Plate =
      @tailrec
      def execRotation(cpt : Long, listState : ListBuffer[String]) : Plate =
        var currCheck : String = this.toString()
         // println("count -- n : [%s] - cpt : [%s]".format(n,cpt))
        if (cpt >= n) then
          // println("n : [%d] - cpt : [%d]".format(n,cpt))
          return this
        else
          val initState : String = this.toString()
          rotateCycle()
          val currState : String = this.toString()
          if (initState == currState) then
           return this
          else 
            if (listState.contains(currState)) then
              val stateStart = listState.indexOf(currState)
              val stateMove = listState.length - stateStart
              val stayUntilEnd = (n - cpt) % stateMove  
              // println("stateStart : [%s] - stateMove : [%s] - stayUntilend : [%s]".format(stateStart,stateMove,stayUntilEnd))
              if (stayUntilEnd == 0) then 
                return execRotation(n,listState)
              else 
                return execRotation(n - stayUntilEnd +1 ,ListBuffer())
            else
              listState += currState
              return execRotation(cpt+1L,listState)
      return execRotation(0,ListBuffer())
      




  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val pl : Plate = Plate(data.map(_.toList).toList)
    pl.moveToNorth()
    if debug then println(pl)
    return pl.getValorisation()

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val pl : Plate = Plate(data.map(_.toList).toList)
    pl.execRotation(1000000000L)
    if debug then println(pl)
    return pl.getValorisation()


@main def runDay14() = 
  val day = Day14()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
