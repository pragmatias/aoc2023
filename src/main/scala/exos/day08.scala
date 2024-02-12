package exos

import fr.pragmatias.tools.Utils
import scala.collection.immutable.HashMap
import scala.annotation.tailrec

class Day08() :

  val dataFileS11 = "day08_step1_1.txt"
  val dataFileS12 = "day08_step1_2.txt"
  val dataFileS2 = "day08_step2.txt"
  val dataFileFull = "day08.txt"

  val rexNet = """(\w{3})""".r

  case class Branch(l : String, r: String)
  case class ReSearch(str : String, cur : String, instr: List[Char],num : Long) :
    override def toString() : String = "ReSearch(%S,%s,%d)".format(str,cur,num)

  def extractInput(line : String) : (String,Branch) =
    val info = rexNet.findAllIn(line).toIndexedSeq
    // println("line : %s - info : %s".format(line,info))
    return (info(0),Branch(info(1),info(2)))

  @tailrec  
  private def navigation(instr:List[Char],hm:HashMap[String,Branch],curr:String,end:String,stepNumber:Long) : Long =
    if (curr == end) {
      return stepNumber
    } else {
      val road : String = instr.head+""
      val branch : Branch = hm.get(curr).getOrElse(Branch("",""))
      val branchValue = if (road == "L") then branch.l else branch.r
      // println("Step (%d) : %s -> %s".format(stepNumber,curr,branchValue))
      navigation(instr.tail:::List(instr.head),hm,branchValue,end,stepNumber+1L )
    }

  @tailrec
  private def searchNextEndingZ(rs : ReSearch, hm : HashMap[String,Branch],stepNumber : Long) : ReSearch =
    if (rs.cur.endsWith("Z") && stepNumber > 0L) {
      return ReSearch(rs.str,rs.cur,rs.instr,rs.num+stepNumber)
    } else {
      val road : String = rs.instr.head+""
      val branch : Branch = hm.get(rs.cur).getOrElse(Branch("Z","Z"))
      val branchValue = if (road == "L") then branch.l else branch.r
      searchNextEndingZ(ReSearch(rs.str,branchValue,rs.instr.tail:::List(rs.instr.head),rs.num),hm,stepNumber+1) 
    }
       
  @tailrec
  private def moveToNextSearch(rs : ReSearch, hm : HashMap[String,Branch],stepToMove : Long) : ReSearch =
    if ( stepToMove == rs.num ) {
      return ReSearch(rs.str,rs.cur,rs.instr,rs.num)
    } else {
      val road : String = rs.instr.head+""
      val branch : Branch = hm.get(rs.cur).getOrElse(Branch("Z","Z"))
      val branchValue = if (road == "L") then branch.l else branch.r
      moveToNextSearch(ReSearch(rs.str,branchValue,rs.instr.tail:::List(rs.instr.head),rs.num+1L),hm,stepToMove) 
    }
 

  def calcGCD(a:Long,b:Long) : Long =
    val keep = a % b
    return if (keep == 0L) then b else calcGCD(b,keep)
  // using the least common multiple for optim step2
  def calcLCM(a:Long, b:Long) : Long = 
    return (a*b) / calcGCD(a,b)

  def navigationS2(instr:List[Char],hm:HashMap[String,Branch]) : Long = 
    val listStart = hm.keys.filter(_.endsWith("A")).map(x => ReSearch(x,x,instr,0L)).toList
    val newListRS = listStart.map(searchNextEndingZ(_,hm,0L))
    val lcm = newListRS.foldLeft(1L)((c,e) => calcLCM(c,e.num))
    return lcm 


  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val instructions : List[Char] = data.head.toList
    val network : HashMap[String,Branch] = data.filter(_.contains("=")).map(extractInput(_)).foldLeft[HashMap[String,Branch]](HashMap())((c,e) => c+e)
    if (debug) {
      println(instructions)
      network.foreach(println(_))
    }
    return navigation(instructions,network,"AAA","ZZZ",0L)

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val instructions : List[Char] = data.head.toList
    val network : HashMap[String,Branch] = data.filter(_.contains("=")).map(extractInput(_)).foldLeft[HashMap[String,Branch]](HashMap())((c,e) => c+e)
    if (debug) {
      println(instructions)
      network.foreach(println(_))
    }
    return navigationS2(instructions,network)
    // return 0


@main def runDay08() = 
  val day = Day08()
  // println("Step1 : Sample S1")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS11,true))
  // println("Step1 : Sample S2")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS12,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS2,true))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,true))
  
