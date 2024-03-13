package fr.pragmatias.exos

import fr.pragmatias.tools.Utils
import scala.runtime.stdLibPatches.language.`3.1`

class Day03() :

  val dataFileS1 = "day03_step1.txt"
  val dataFileFull = "day03.txt"

  val regexDecim = """(\d+)""".r
  val regexSigne = """([^\d\.])""".r
  val regexStar = """(\*)""".r


  case class Position(line:Int,start:Int,end:Int)
  case class Decim(decim:Int,pos:Position)
  case class Signe(signe:String,pos:Position)

  def decimIsNearListSigne(decim:Decim, listSigne:List[Signe]) : Boolean = 
    return listSigne.filter(x => decimIsNearSigne(decim,x)).length > 0

  def getListDecimNearSigne(signe:Signe, listDecim:List[Decim]) : List[Decim] =
    return listDecim.filter(x => decimIsNearSigne(x,signe))

  def decimIsNearSigne(decim:Decim,signe:Signe) : Boolean = 
    return (signe.pos.line == decim.pos.line && (signe.pos.start == decim.pos.start-1 || signe.pos.start == decim.pos.end))
            || ((signe.pos.line == decim.pos.line+1 || signe.pos.line == decim.pos.line-1)
                  && (signe.pos.start >= decim.pos.start-1 && signe.pos.start <= decim.pos.end)
              )

  def runStep1(p: os.Path, f: String, debug:Boolean) : Int =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    // val tmp = data.map(x => x.split('.'))
    val listDecim = data.map(x => regexDecim.findAllMatchIn(x).map(y => (y.matched.toInt,(y.start,y.end))))
                        .foldLeft[(Int,List[Decim])]((1,List()))((cuml,elt) => {
                          (cuml._1+1,cuml._2:::elt.map(x => Decim(x._1,Position(cuml._1,x._2._1,x._2._2))).toList)
                        })._2

    val listSigne = data.map(x => regexSigne.findAllMatchIn(x).map(y => (y.matched,(y.start,y.end))))
                        .foldLeft[(Int,List[Signe])]((1,List()))((cuml,elt) => {
                          (cuml._1+1,cuml._2:::elt.map(x => Signe(x._1,Position(cuml._1,x._2._1,x._2._2))).toList)
                        })._2
    if (debug) {
      // tmp.foreach(println(_))
      println("debug listDecim")
      // listDecim.foreach(x => { x.foreach(y => {print(y);print("|")});println("")})
      println(listDecim)
      println("debug listSigne")
      // listSigne.foreach(x => { x.foreach(y => {print(y);print("|")});println("")})
      println(listSigne)
    }

    val resTmp = listDecim.filter(decimIsNearListSigne(_,listSigne))
    // println(resTmp)
    return resTmp.foldLeft(0)((cuml,elt)=>cuml+elt.decim)

  def runStep2(p: os.Path, f: String, debug:Boolean) : Int = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    // val tmp = data.map(x => x.split('.'))
    val listDecim = data.map(x => regexDecim.findAllMatchIn(x).map(y => (y.matched.toInt,(y.start,y.end))))
                        .foldLeft[(Int,List[Decim])]((1,List()))((cuml,elt) => {
                          (cuml._1+1,cuml._2:::elt.map(x => Decim(x._1,Position(cuml._1,x._2._1,x._2._2))).toList)
                        })._2

    val listStar = data.map(x => regexStar.findAllMatchIn(x).map(y => (y.matched,(y.start,y.end))))
                        .foldLeft[(Int,List[Signe])]((1,List()))((cuml,elt) => {
                          (cuml._1+1,cuml._2:::elt.map(x => Signe(x._1,Position(cuml._1,x._2._1,x._2._2))).toList)
                        })._2
    if (debug) {
      // tmp.foreach(println(_))
      println("debug listDecim")
      // listDecim.foreach(x => { x.foreach(y => {print(y);print("|")});println("")})
      println(listDecim)
      println("debug listSigne")
      // listSigne.foreach(x => { x.foreach(y => {print(y);print("|")});println("")})
      println(listStar)
    }

    val resTmp = listStar.map(x => (x,getListDecimNearSigne(x,listDecim))).filter(x => x._2.length>1)
    // println(resTmp)
    return resTmp.foldLeft(0)((cuml,elt)=>cuml+elt._2.foldLeft(1)((c,e) => c*e.decim))


@main def runDay03() = 
  val day = Day03()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,true))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,true))
  
