package exos

import fr.pragmatias.tools.Utils
import scala.collection.immutable.HashMap
import scala.annotation.tailrec

class Day19() :

  val dataFileS1 = "day19_step1.txt"
  val dataFileFull = "day19.txt"

  enum Signe :
    case Sup,Inf,InfE,SupE
    def inverse : Signe = this match
      case Signe.Sup => Signe.InfE
      case Signe.Inf => Signe.SupE
      case Signe.InfE => Signe.Sup
      case Signe.SupE => Signe.Inf
    

  trait Rule
  case class RuleWithCond(cond:Condition,res:String) extends Rule
  case class RuleElse(res:String) extends Rule
  case class Condition(rated:String,signe:Signe,value:Int) :
    def checkCondition(command:List[Command]) : Boolean =
      val checkV = command.filter(x => x.rated == rated).head.value
      return (signe match
          case Signe.Sup => checkV > value
          case Signe.Inf => checkV < value
          case Signe.InfE => checkV <= value
          case Signe.SupE => checkV >= value)
    def inverse : Condition = Condition(rated,signe.inverse,value)
  case class Command(rated:String,value:Int) 
  case class Borne(inf:Int,sup:Int)
  case class CommandBorne(var hmB:HashMap[String,Borne])



  def extractRule(s:String) : (String,List[Rule]) =
    val tmp = s.split('{')
    val ruleName = tmp(0)
    val rulesRaw = tmp(1).substring(0,tmp(1).length-1).split(",")
    val rulesRes = rulesRaw.map(r => {
      val sign : Option[Signe] = if r.contains(">") then Option(Signe.Sup) else if r.contains("<") then Option(Signe.Inf) else None
      val conds = r.split(":")
      sign match
        case Some(si) => RuleWithCond(Condition(conds(0).slice(0,1),si,conds(0).slice(2,conds(0).length).toInt),conds(1))
        case _ => RuleElse(r)
    }).toList
    return (ruleName,rulesRes)

  def extractRules(data : List[String]) : HashMap[String,List[Rule]] =
    return data.filter(d => !d.startsWith("{") && !d.isEmpty).map(extractRule).foldLeft(HashMap.empty[String,List[Rule]]) :
      case (hm,(name,lr)) => HashMap(name -> lr)++hm
  def extractCommand(s:String) : List[Command] = 
    return  s.slice(1,s.length-1).split(',').map(x => Command(x.slice(0,1),x.slice(2,x.length).toInt)).toList

  def extractCommands(data : List[String]) : List[List[Command]] =
    return data.filter(d => d.startsWith("{")).map(extractCommand).toList

  @tailrec
  final def execRulesOnCommand(nameRule:String,lc:List[Command],hmR:HashMap[String,List[Rule]]) : Boolean =
    if (nameRule == "A") then true
    else if (nameRule == "R") then false
    else 
      val command = lc.head
      val lr = hmR(nameRule)
      val (_,nr) : (Boolean, String) = lr.foldLeft((false,"")):
        case((valid,name),r) => 
          if valid then (valid,name)
          else 
            r match
            case RuleWithCond(cond,rres) => if (cond.checkCondition(lc)) then (true,rres) else (valid,name)
            case RuleElse(rres) => (true,rres) 
      return execRulesOnCommand(nr,lc,hmR)

  def calculStep1(llc:List[List[Command]],hmR:HashMap[String,List[Rule]]):Long =
    return llc.filter(lc => execRulesOnCommand("in",lc,hmR)).map(lc => lc.map(_.value).sum).sum

  def compileCondFromRules(lr:List[Rule]) : List[(String,List[Condition])] =
    return lr.foldLeft[List[(String,List[Condition])]](List()):
      case (cuml,rs) => rs match
        case RuleWithCond(cond,res) => 
          if (cuml.isEmpty) then (res,List(cond))::cuml
          else 
            val cumlConds = cuml.head._2
            (res,cond::(cumlConds.head.inverse)::cumlConds.tail)::cuml
        case RuleElse(res) => 
          val cumlConds = cuml.head._2
          (res,cumlConds.head.inverse::cumlConds.tail)::cuml
      


  def extractPathToSpecificRule(nameRuleCurr:String, nameRuleEnd:String, hmR:HashMap[String,List[Rule]],res:List[Condition]) : List[List[Condition]] = 
    if nameRuleCurr == nameRuleEnd then 
      List(res)
    else 
      if List("A","R").contains(nameRuleCurr) then 
        List()
      else
        val rules = hmR(nameRuleCurr)
        val lconds = compileCondFromRules(rules)
        val resTmp = lconds.flatMap(x => extractPathToSpecificRule(x._1,nameRuleEnd,hmR,x._2:::res))
        return resTmp

  def appConditionOnCommandBorne(com : CommandBorne, cond : Condition) : CommandBorne =
    val borne = com.hmB(cond.rated)
    val other = com.hmB.filter(_._1!=cond.rated)
    val nb : Borne = cond.signe match
      case Signe.Inf => if (borne.sup >= cond.value-1) then Borne(borne.inf,cond.value-1) else borne
      case Signe.Sup => if (borne.inf <= cond.value+1) then Borne(cond.value+1,borne.sup) else borne 
      case Signe.InfE => if (borne.sup >= cond.value) then Borne(borne.inf,cond.value) else borne
      case Signe.SupE => if (borne.inf <= cond.value) then Borne(cond.value,borne.sup) else borne
    val n : HashMap[String,Borne]= HashMap((cond.rated,nb))
    return CommandBorne(n++other)
    

    
         
  def calculStep2(hmR:HashMap[String,List[Rule]]) : Long = 
    val llc = extractPathToSpecificRule("in","A",hmR,List()).filter(!_.isEmpty)
    val llcom = llc.map(lc => lc.foldLeft(CommandBorne(HashMap(("x",Borne(1,4000)),("a",Borne(1,4000)),("m",Borne(1,4000)),("s",Borne(1,4000))))):
                    case (com,cond) => appConditionOnCommandBorne(com,cond))
                   .map(lcom => lcom.hmB.foldLeft(1L):
                    case (res,(s,v)) => res * (1 + v.sup - v.inf))
    val res = llcom.foldLeft(0L)((c,e) => c+e)
    return res




  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val hmRules = extractRules(data.toList)
    val lCom = extractCommands(data.toList)
    if debug then 
      hmRules.foreach(println)
      lCom.foreach(println)
    return calculStep1(lCom,hmRules)

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val hmRules = extractRules(data.toList)
    if debug then
    hmRules.foreach(println)
    return calculStep2(hmRules)


@main def runDay19() = 
  val day = Day19()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  println("Step2 : Full")
  println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
