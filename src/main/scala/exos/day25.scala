package fr.pragmatias.exos

import fr.pragmatias.tools.Utils
import scala.collection.immutable.HashMap
import scala.collection.immutable.BitSet

class Day25() :

  val dataFileS1 = "day25_step1.txt"
  val dataFileFull = "day25.txt"

  type Id = Int
  type Vertices = BitSet
  type Weight = Map[Id, Map[Id,Int]]
  
  class Dict(ls : List[String]) :
    val hmStringToId : HashMap[String,Id] = ls.distinct.zipWithIndex.foldLeft(HashMap.empty):
      case (hm,(s,i)) => HashMap((s -> i))++hm
    val hmIdToString : HashMap[Int,String] = hmStringToId.foldLeft(HashMap.empty):
      case (hm,(s,i)) => HashMap((i -> s))++hm
    def getId(s : String) : Int = hmStringToId(s)
    def getString(i: Int) : String = hmIdToString(i)



  case class Graph(v : Vertices, nodes: Map[Id,Vertices], w:Weight) :

    
    def shrink(s:Id, t:Id): Graph =
      def fetch(x:Id) = 
        w(x).view.filterKeys(y => y != s && y != t)

      val prunedW = (w - t).view.mapValues(_ - t).toMap

      val fromS = fetch(s).toMap
      val fromT = fetch(t).map:
        case( y, w0 ) => y -> (fromS.getOrElse(y,0) + w0)
      val mergedWeights = fromS ++ fromT

      val reverseMerged = mergedWeights.view.map:
       case (y, w0) => y -> (prunedW(y) + (s-> w0))

      val v1 = v - t 
      val w1 = prunedW + (s -> mergedWeights) ++ reverseMerged 
      val nodes1 = nodes -t + (s -> (nodes(s) ++ nodes(t)))
      Graph(v1,nodes1,w1)

  





  def extractConnectFromInput(data:List[String]) : HashMap[String,List[String]] =
    return data.map(s => s.split(":"))
               .map(ls => (ls(0).trim,ls(1).trim.split(" ").map(_.trim).toList))
               .foldLeft(HashMap.empty):
                case (hm,(name,lc)) => HashMap((name -> lc)) ++ hm 
  

  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val listConnect : HashMap[String,List[String]] = extractConnectFromInput(data.toList)
    val dict : Dict = Dict(listConnect.flatMap(c => c._1::c._2).toList)
    
    if debug then 
      listConnect.foreach(println)
      dict.hmIdToString.foreach(println)
      dict.hmStringToId.foreach(println)
    return 0


@main def runDay25() = 
  val day = Day25()
  println("Step1 : Sample")
  println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  
