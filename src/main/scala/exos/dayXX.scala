package fr.pragmatias.exos

import fr.pragmatias.tools.Utils

class DayXX() :

  val dataFileS1 = "dayXX_step1.txt"
  val dataFileFull = "dayXX.txt"


  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    return 0

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    return 0


@main def runDayXX() = 
  val day = DayXX()
  println("Step1 : Sample")
  println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step2 : Full")
  // println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
