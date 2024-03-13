package fr.pragmatias.exos

import fr.pragmatias.tools.Utils
import scala.annotation.tailrec

class Day09() :

  val dataFileS1 = "day09_step1.txt"
  val dataFileFull = "day09.txt"

  def findNextNumber(li : List[Long]) : Long =
    val lastNumber = li.last
    val newL = li.zip(li.tail).map((x,y) => y-x)
    if (newL.filter(_ != 0L).length == 0) {
      return lastNumber
    } else {
      return lastNumber + findNextNumber(newL)
    }

  def findFirstNumber(li : List[Long]) : Long = 
    val firstNumber = li.head
    val newL = li.zip(li.tail).map((x,y) => y-x)
    if (newL.filter(_ != 0L).length == 0) {
      return firstNumber
    } else {
      return firstNumber - findFirstNumber(newL)
    }

  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val lc : List[List[Long]] = data.map(_.split(" ").map(_.trim.toLong).toList).toList
    // println(lc)
    val res = lc.map(findNextNumber(_)).foldLeft(0L)((c,e) => c+e)
    return res

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val lc : List[List[Long]] = data.map(_.split(" ").map(_.trim.toLong).toList).toList
    // println(lc)
    val res = lc.map(findFirstNumber(_)).foldLeft(0L)((c,e) => c+e)
    return res


@main def runDay09() = 
  val day = Day09()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
