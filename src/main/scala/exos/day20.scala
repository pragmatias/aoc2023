package exos

import fr.pragmatias.tools.Utils
import scala.collection.mutable.HashMap
import scala.annotation.tailrec

class Day20() :

  val dataFileS11 = "day20_step1_1.txt"
  val dataFileS12 = "day20_step1_2.txt"
  val dataFileFull = "day20.txt"

  enum Pulse:
    case Low,High
  
  trait Module :
    val listConnection :List[String]
    val name : String
  case class FlipFlop(name:String,active:Boolean=false,listConnection:List[String]) extends Module:
    def execWithPulse(pulse:Pulse) : (FlipFlop,List[Flow]) =
      pulse match
      case Pulse.High => return (this,List())
      case Pulse.Low => 
        val pulseRes = if active then Pulse.Low else Pulse.High
        return (this.copy(active = !this.active),listConnection.map(m => Flow(name,pulseRes,m)))
  case class Conjonction(name:String,listConnection:List[String],listDep:List[String]=List()) extends Module :
    val knowPulse: HashMap[String,Pulse] = listDep.foldLeft(HashMap())((c,e) => c++HashMap((e,Pulse.Low)))
    def execWithPulse(oldFlow : List[Flow]) : List[Flow] = 
      val lres = listDep.map( x => oldFlow.find( y => y.nameEnd == name && y.nameStart == x).getOrElse(Flow(x,knowPulse(x),name)))
      //update knowPulse
      lres.foreach({ f => knowPulse(f.nameStart) = f.pulse })
      // println(lres)
      val pulse = if (lres.filter(_.pulse == Pulse.High).length == lres.length) then Pulse.Low else Pulse.High
      return listConnection.map(c => Flow(name,pulse,c)).toList
  case class UntypedModule(name:String,listConnection:List[String]) extends Module
  case class Flow(nameStart:String,pulse:Pulse=Pulse.Low,nameEnd:String)


  var storeState : List[(State,(Long,Long))] = List()
  case class State(workflow:String)
  
  def getStateFromWorkflow(hmModule : HashMap[String,Module]) : String =
    return hmModule.map(x => "("+x._1+","+x._2+")").mkString("\n")

  def extractModule(s:String) : (String,Module) =
    val tmp = s.split("->").take(2)
    val nameModule = tmp(0).trim
    val nameModuleSpec = nameModule.slice(1,nameModule.length)
    val listModule = tmp(1).split(",").map(_.trim).toList
    if (nameModule.slice(0,1) == "&") then 
      //Conjonction
      (nameModuleSpec,(Conjonction(name = nameModuleSpec,listConnection = listModule)))
    else 
      if (nameModule.slice(0,1) == "%") then
        //FlipFlop
        (nameModuleSpec,(FlipFlop(name = nameModuleSpec, listConnection = listModule)))
      else 
        //broadcast
        (nameModule,(UntypedModule(name = nameModule , listConnection = listModule)))
  
  def extractModules(data:List[String]) : HashMap[String,Module] =
    val hmEmptyMod : HashMap[String,Module] = HashMap.empty
    val hmResMod = data.map( extractModule ).foldLeft(hmEmptyMod)((c,e) => HashMap((e))++c)
    // get UntypedModule
    val untypedModule = hmResMod.flatMap(x => x._2.listConnection).toSeq.distinct.filter(x => !hmResMod.contains(x))
    // keep Modules 
    val hmResModTmp = hmResMod.map(x => x match 
      case (name,Conjonction(n,lc,ld)) => (name,Conjonction(name,lc,hmResMod.filter(c => c._2.listConnection.contains(name)).map(c => c._1).toList))
      case _ => x)
    val hmResModule = untypedModule.foldLeft(hmResModTmp)((c,e) => HashMap((e,UntypedModule(e,List())))++c)
    return hmResModule




  @tailrec
  final def runWorkflow(lFlow : List[Flow], hmModule:HashMap[String,Module], lres : List[Flow]): (Long,Long,List[Flow]) =
    if (lFlow.isEmpty) then
      // println("In =======")
      // lres.reverse.foreach(println)
      // println("OUT ======")
      // count flow
      return (lres.filter(_.pulse == Pulse.Low).length, lres.filter(_.pulse == Pulse.High).length,lres)
    else 
      val flow = lFlow.head
      val mod = hmModule(flow.nameEnd)
      val resTmp : List[Flow] = mod match
        case conj : Conjonction => conj.execWithPulse(flow::lres)
        case fpfp : FlipFlop => 
          val (ff,lp) = fpfp.execWithPulse(flow.pulse)
          //update module on/off
          hmModule(fpfp.name) = ff
          lp
        case UntypedModule(name,lc) => lc.map(x => Flow(name,flow.pulse,x)).toList

      return runWorkflow(lFlow.tail:::resTmp,hmModule,flow::lres)
      


  def calculStep1(hmModule:HashMap[String,Module]) : Long = 
    val flowButton = Flow("button",Pulse.Low,"broadcaster")
    def calculStep1(start:Int,end:Int,res:(Long,Long)) : (Long,Long) =
      if (start >= end) then 
        return res 
      else 
        val startState = getStateFromWorkflow(hmModule)
        // println(storeState.length)
        val ind = storeState.indexWhere(x => x._1.workflow == startState) + 1
        if ind > 0 then 
          // println("FOUUUUN STTTAATTE : "+ind)
          val hold = (end - start) % ind
          val mult = (end - start - hold) / ind
          val nStart = end - hold
          // println("ind : %s - hold : %s - mult : %s - nStart : %s - Start : %s - End : %s".format(ind,hold,mult,nStart,start,end))
          val (resLTmp1,resHTmp2) = storeState.slice(0,ind).foldLeft((0L,0L))((c,e) => (c._1+e._2._1,c._2+e._2._2))
          val (resL,resH) = (resLTmp1*mult,resHTmp2*mult)
          if hold == 0 then 
              return calculStep1(nStart,end,(res._1+resL,res._2+resH))
          else
            val (resLTmp,resHTmp,_) = runWorkflow(List(flowButton),hmModule,List())
            return calculStep1(nStart+1,end,(res._1+resL+resLTmp,res._2+resH+resHTmp))

        else 
          val (resLowTmp,resHighTmp,_) = runWorkflow(List(flowButton),hmModule,List()) 
          storeState ::= (State(startState),(resLowTmp,resHighTmp))
          return calculStep1(start+1,end,(res._1+resLowTmp,res._2+resHighTmp))
    val (resL,resH) = calculStep1(0,1000,(0L,0L))
    // storeState.foreach(println) 
    return resL * resH

  def calculStep2(hmModule:HashMap[String,Module]) : Long = 
    val flowButton = Flow("button",Pulse.Low,"broadcaster")
    // get dependance from rx 
    val rxDep = hmModule.filter(x => x._2.listConnection.contains("rx")).head
    println(rxDep)
    val subDeps = hmModule.filter(x => x._2.listConnection.contains(rxDep._1)).map(_._1).toList

    def calculStep2(iter:Long,lsubDeps:List[String],lsubRes:List[(String,Long)]) : Long =
      if lsubDeps.length == lsubRes.length then 
        val res = lcm(lsubRes.map(_._2))//calcul
        println(lsubRes)
        return res
      else 
        val (_,_,lf) = runWorkflow(List(flowButton),hmModule,List())
        val lres = lsubDeps.filter(x => lf.filter(f => f.nameStart == x  && f.pulse == Pulse.High && f.nameEnd == rxDep._1).length > 0 
                                                  && lsubRes.filter(s => s._1 == x).length == 0)
                          .map(s => (s,iter))
        calculStep2(iter+1,lsubDeps,lres:::lsubRes)
    return calculStep2(1L,subDeps,List())

  def lcm(list: List[Long]): Long =
    list.foldLeft(1L)((a, b) => b * a / gcd(a, b))

  @tailrec
  final def gcd(x: Long, y: Long): Long =
    if y == 0 then x else gcd(y, x % y)

  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val hmModule = extractModules(data.toList)
    if debug then
      hmModule.foreach(println)
    val res = calculStep1(hmModule)
    return res

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val hmModule = extractModules(data.toList)
    if debug then
      hmModule.foreach(println)
    val res = calculStep2(hmModule)
    return res


@main def runDay20() = 
  val day = Day20()
  // println("Step1 : Sample (1)")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS11,true))
  // println("Step1 : Sample (2)")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS12,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
