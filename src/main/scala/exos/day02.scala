package fr.pragmatias.exos

import fr.pragmatias.tools.Utils
// import _root_.tools.{dataFullPath,dataSamplePath}


class Day02() :

  val dataFileS1 = "day02_step1.txt"
  val regexGame = """^[^\d]*(\d+)[^\d]*$""".r
  val regexJet = """^\s*(\d+)\s+(\w+)\s*$""".r

  class Game(val line : String): 
    val tmp : Array[String] = line.split(":")
    val number : Int = tmp(0).trim() match {
      case regexGame(num) => num.toInt
      case _ => 0
    }

    val listJet : Array[Jet] = tmp(1).split(";").map(x => new Jet(x))

    val jetMin : Array[(Int,String)] = 
      listJet.tail.foldLeft(listJet.head.detail)((a,b) => Array.concat(a,b.detail))
        .groupBy(_._2).map(x => (x._1,x._2.map(_._1).foldLeft(0)((cuml,elt) => if (cuml < elt) { elt } else { cuml })))
        .toArray.map(x => (x._2,x._1))
      


    override def toString() : String =
      "Game n°%d".format(number) +" : "+ listJet.mkString(" - ") +"\n\t Min -> "+jetMin.map(x => "(%d,%s)".format(x._1,x._2)).mkString(",")

    def gameValidWithJet(j : Jet) : Boolean =
      listJet.filter( x => ! x.isValidWith(j) ).length < 1

    def powerJetMin() : Int = jetMin.foldLeft(1)((cuml,elt) => cuml * elt._1)


  class Jet(val line : String) :
    var detail : Array[(Int,String)] = line.split(",").map(x => {
      x match {
        case regexJet(num,color) => (num.trim.toInt,color.trim)
        case _ => (0,"")
      }
    })
    
    def isValidWith(j : Jet) : Boolean =
      j.detail.filter((n,c) => (detail.filter((n1,c1) => (c1 == c) && (n1 > n)).length > 0)).length == 0 
    override def toString() : String = 
      "Jet --> "+ detail.map(x => "(%d,%s)".format(x._1,x._2)).mkString(",") 



  def runStep1(p : os.Path, f : String, debug : Boolean) : Int =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val listGames = data.map(x => Game(x))
    if (debug) {
      listGames.foreach(println(_))
    }
    val jetTest = Jet("12 red, 13 green, 14 blue")
    if (debug) {
      println("Jet ==> %s".format(jetTest))
    }
    val listGamesOk = listGames.filter(x => x.gameValidWithJet(jetTest))
    if (debug) {
      listGamesOk.foreach(println(_))
    }
    return listGamesOk.foldLeft(0)((a,b) => a+b.number)

  def runStep2(p : os.Path, f:String, debug:Boolean) : Int =
    val data : geny.Generator[String] = os.read.lines.stream(p/f)
    val listGames = data.map(x => Game(x))
    if (debug) { listGames.foreach(println(_))}
    return listGames.foldLeft(0)((cuml,elt) => cuml + elt.powerJetMin())


@main def runDay02() =
    val day = Day02()
    // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
    // println(day.runStep1(Utils.dataFullPath,day.dataFileS1,false))
    // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
    println(day.runStep2(Utils.dataFullPath,day.dataFileS1,false))
