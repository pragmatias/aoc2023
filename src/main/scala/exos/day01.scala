package fr.pragmatias.exos

import fr.pragmatias.tools.Utils
import scala.compiletime.ops.boolean
import scala.util.matching.Regex

class Day01() : 
  val dataFileS1 = "day01_step1.txt"
  val dataFileS2 = "day01_step2.txt"
  val dataFileFull = "day01.txt"

  val twoDigitRex = raw"^[^\d]*(\d).*(\d)[^\d]*".r
  val oneDigitRex = raw"^[^\d]*(\d)[^\d]*".r


  // val twoDigitRex2 = raw"^[^\d]*(\d).*(\d)[^\d]*".r
  val translateDigit : Seq[(String,String)]= Seq(("one","1")
                                              ,("two","2")
                                              ,("three","3")
                                              ,("four","4")
                                              ,("five","5")
                                              ,("six","6")
                                              ,("seven","7")
                                              ,("eight","8")
                                              ,("nine","9"))

  val listNumber = "("+translateDigit.map(x => x._1).mkString("|")+"|\\d)"

  val twoDigitRexDeb = ("""^"""+listNumber).r
  val twoDigitRexEnd = (listNumber+"""$""").r


  def runStep1(p: os.Path, f : String , debug : Boolean) : Int =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    // For debug
    if (debug) {
      println("Debug (Step n°1)")
      data.foreach(x => println(x))
    }
    // Exercice
    val resultTmp = data.map(x => x.match { 
      case twoDigitRex(dd,df) => dd + df
      case oneDigitRex(dd) => dd + dd
      case _ => "0"
    })
    
    return resultTmp.foldLeft(0)((a,b) => a +  b.toInt)



  def runStep2(p : os.Path, f : String, debug : Boolean) : Int = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    // println(listNumber)
    val resultTmp = data.map(x => getNumber(x,None,None)).map(x => translateNumber(x._1) + translateNumber(x._2))
    if (debug) {
      resultTmp.foreach(println(_))
    }
    return resultTmp.foldLeft(0)((a,b) => a +  b.toInt)

  def getNumber(s : String, a : Option[String], b : Option[String]) : (String,String) = {
    if ( s.length >= 1 && (a.isEmpty || a.getOrElse("0") == "0")) {
      val res = twoDigitRexDeb.findFirstIn(s)
      if (res.isEmpty) {
        return getNumber(s.substring(1,s.length), None, b)
      } else {
        return  getNumber(s, res, b)
      }
    } else if (s.length >= 1 && (b.isEmpty || b.getOrElse("0") == "0")) {
      val res = twoDigitRexEnd.findFirstIn(s)
      if (res.isEmpty) {
        getNumber(s.substring(0,s.length-1), a, res)
      } else {
        return getNumber(s, a, res)
      }
    } else {
      return (a.getOrElse("0") , b.getOrElse("0"))
    }
  }

  def translateNumber(s : String) : String = {
    val res = translateDigit.filter(_._1 == s).map(_._2).toList
    if (res.length < 1) {
      return s
    } else {
      return res.head
    }
  }


@main def run() =
  val day = Day01()
  // println("Exercise with Sample data :")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS2,true))
  println("Exercise with Full data :")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false)) 
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
