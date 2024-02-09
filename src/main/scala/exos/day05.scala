package exos

import fr.pragmatias.tools.Utils
import scala.collection.immutable.HashMap
import os.isDir

class Day05() :

  val dataFileS1 = "day05_step1.txt"
  val dataFileFull = "day05.txt"

  val rexDecim = """(\d+)""".r
  val rexMap = """(.*) map:""".r
  val rexDecimPair = """(\d+ \d+)""".r

  val hStep = HashMap((1,"seed-to-soil")
                      ,(2,"soil-to-fertilizer")
                      ,(3,"fertilizer-to-water")
                      ,(4,"water-to-light")
                      ,(5,"light-to-temperature")
                      ,(6,"temperature-to-humidity")
                      ,(7,"humidity-to-location")
                      )

  case class RangeSeed(start:Long, rng:Long)
    

  class Transfo(src : Long, dest : Long, rng:Long) :
    override def toString() : String =
      "Transfo(%d,%d,%d)".format(src,dest,rng)
  
    def isInSrc(cur:Long) : Boolean =
      (cur >= src) && (cur < src + rng)
    def getDest(cur:Long) : Long = 
      if (isInSrc(cur)) { (dest + (cur - src )) } else { cur }

    def extractSeedDest(seed:RangeSeed) : (List[RangeSeed],List[RangeSeed]) =
      val stSeed = seed.start
      val enSeed = seed.start+seed.rng-1
      val stTr = src
      val enTr = src+rng-1
      // println(" starting : %d to %d".format(stSeed,enSeed))
      // println(" compare : %d to %d".format(stTr,enTr))
      // println(" dest : %d to %d".format(dest,dest+rng-1))

      if (stSeed >= stTr && enSeed <= enTr) {
      // if seed is in transfo
       return (List(RangeSeed(dest+(stSeed-stTr),(enSeed-stSeed+1))),List())
      } else if (stSeed <= stTr && enSeed >= stTr) { 
      // if seed is partially in transfo (before start)
        val rangeSeedStart = RangeSeed(stSeed,stTr-stSeed+1)
        if (enSeed <= enTr) {
          return (List(RangeSeed(dest,enSeed-stTr+1)),List(rangeSeedStart))
        } else {
          return (List(RangeSeed(dest,rng)),List(rangeSeedStart,RangeSeed(enTr,enSeed-enTr+1)))
        }
      } else if (stSeed <= enTr && enSeed >= enTr) {
       // if seed is partilly in transfo (after start)
        return (List(RangeSeed(dest+(stSeed-stTr),enTr-stSeed+1)),List(RangeSeed(enTr,enSeed-enTr+1)))
      } else if (stSeed <= stTr && enSeed >= enTr) {
        // if transfo is in seed
        return (List(RangeSeed(dest,rng)),List(RangeSeed(stSeed,stTr-stSeed),RangeSeed(enTr,enSeed-enTr+1)))
      } else {
        // if seed is not in transfo
        return (List(),List(seed))
      }

  def extractSeedFromInput(line : String) : List[Long] =
    return rexDecim.findAllIn(line).map(_.toLong).toList
 
  def extractSeedPairFromInput(line : String) : List[RangeSeed] =
    return rexDecimPair.findAllIn(line).map(_.split(" ")).map(x => RangeSeed(x(0).toLong,x(1).toLong)).toList

  def extractInfoFromInput(lines : IndexedSeq[String], key : String, content : List[String]) : HashMap[String,List[Transfo]] =
    val line = lines.head
    if (lines.length > 1) {
      if (line.isEmpty) { return extractInfoFromInput(lines.tail,key,content) }
      val test = rexMap.findFirstIn(line)
      if (test.isEmpty) {
        extractInfoFromInput(lines.tail,key,line::content)
      } else {
        if (key == "") {
          extractInfoFromInput(lines.tail,test.getOrElse("").split(" ")(0),List())
        } else {
          extractInfoFromInput(lines.tail,test.getOrElse("").split(" ")(0),List()) + (key -> buildSourceToDestCC(content))
        }
      }
    } else {
      return HashMap((key,buildSourceToDestCC(line::content)))
    }

  def buildSourceToDestCC(lines : List[String]) : List[Transfo] =
    lines.map(x => rexDecim.findAllIn(x).map(_.toLong).toList).map(x => Transfo(x(1),x(0),x(2)))

  def searchIntoHash(start:Long,stepCur:Int,stepEnd:Int,hm:HashMap[String,List[Transfo]]) : Long =
    if (stepCur > stepEnd) { 
      return start 
    } else {
      val steplib = hStep.get(stepCur).getOrElse("")
      val newStepContent = hm.get(steplib).getOrElse(List())
      val newStartList = newStepContent.filter(_.isInSrc(start)).map(_.getDest(start))
      val newStart = if (newStartList.length > 0) { newStartList.head } else { start }
      // println("current : %d - update : %d - step : %d".format(start,newStart,stepCur))
      return searchIntoHash(newStart,stepCur+1,stepEnd,hm)
    }

  def extractDest(listSeed:List[RangeSeed],listTr:List[Transfo],listReSeed:List[RangeSeed],listRes: List[RangeSeed]) : List[RangeSeed] =
    if (listSeed.length > 0 && listTr.length > 0) {
      val tr : Transfo = listTr.head
      val seed : RangeSeed = listSeed.head
      // println("extract 1 : seed = "+listSeed)
      // println("extract 1 : transfo = "+tr)
      val (listDest,listNewSeed) : (List[RangeSeed],List[RangeSeed]) = tr.extractSeedDest(seed)
      // println("extract 1.1 "+listDest)
      // println("extract 1.2 "+listNewSeed)
      return extractDest(listSeed.tail,listTr,listReSeed:::listNewSeed,listRes:::listDest)
    } else if (listSeed.length == 0 && listTr.length >= 1) {
      // println("extract 2 : "+listSeed)
      return extractDest(listReSeed,listTr.tail,List(),listRes)
    } else {
      // println("extract 3 : " + listSeed)
      return listRes:::listSeed
    }

  def searchPairsIntoHash(seeds:List[RangeSeed], stepCur:Int, stepEnd: Int, hm:HashMap[String,List[Transfo]]) : List[RangeSeed] =
    if (stepCur > stepEnd) {
      return seeds
    } else {
      val steplib : String = hStep.get(stepCur).getOrElse("")
      val newStepContent : List[Transfo] = hm.get(steplib).getOrElse(List())
      // println("Before extract")
      val res : List[RangeSeed] = extractDest(seeds,newStepContent,List(),List())
      // println("testing : "+res)
      return searchPairsIntoHash(res,stepCur+1,stepEnd,hm)
    }


  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : IndexedSeq[String] = os.read.lines(p / f)
    val seeds : List[Long] = extractSeedFromInput(data.head)
    if (debug) {
      println("Seeds : "+seeds)
    }
    val hmInfo = extractInfoFromInput(data.tail,"",List())
    if (debug) {
      // data.foreach(println(_)) 
      // for( l <- data ) { println(l) }
      hmInfo.foreach(println(_))
    }
    val res = seeds.map(x => (x,searchIntoHash(x,1,7,hmInfo)))
    if (debug) {
      res.foreach(println(_))
    }
    val resFinal = res.tail.foldLeft(res.head)((c,e) => { if (c._2 > e._2) { e } else { c } })
  
    return resFinal._2

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : IndexedSeq[String] = os.read.lines(p / f)
    val seeds : List[RangeSeed] = extractSeedPairFromInput(data.head)
    
    if (debug) {
      println("Seeds : "+seeds)
    }
    val hmInfo = extractInfoFromInput(data.tail,"",List())
    if (debug) {
      hmInfo.foreach(println(_))
    }
    val res : List[RangeSeed] = seeds.flatMap(x => searchPairsIntoHash(List(x),1,7,hmInfo))
    if (debug) {
      // res.foreach(println(_))
    }
    val resFinal : RangeSeed = res.tail.foldLeft(res.head)((c,e) => { if (c.start > e.start) { e } else { c } })
  
    return resFinal.start


@main def runDay05() = 
  val day = Day05()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,true))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
