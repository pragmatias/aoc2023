package exos

import fr.pragmatias.tools.Utils
import scala.runtime.stdLibPatches.language.`3.3`
import scala.runtime.stdLibPatches.language.`3.2`

class Day07() :

  val dataFileS1 = "day07_step1.txt"
  val dataFileS2 = "day07_step2.txt"
  val dataFileFull = "day07.txt"

  enum HandType : 
    case FiveKind, FourKind, FullHouse, ThreeKind, TwoPair, OnePair, HighCard

  case class Input(h:Hand,v:Int) :
    def isBetterThan(i:Input) : Boolean =
      val test = h.isBetterThan(i.h)
      // println("Compare %s - %s = %s".format(h,i.h,test))
      return test

  class Hand(val hand:String) :
    val typeH : HandType = getHandType()
    private def getHandType() : HandType = 
      val tmp = hand.groupBy(identity).map(x => (x._1,x._2.length)).toList 
      return tmp.length match 
        case 1 => HandType.FiveKind
        case 2 => { 
                    if (tmp.filter(_._2 == 4).length == 1) { 
                      HandType.FourKind 
                    } else {
                      HandType.FullHouse
                    } 
                  }
        case 3 => {
                    if (tmp.filter(_._2 == 3).length == 1) {
                      HandType.ThreeKind
                    } else {
                      HandType.TwoPair
                    }
                  }
        case 4 => HandType.OnePair
        case _ => HandType.HighCard

    def getValue(card:Char) : Int = 
      return card+"" match
              case "A" => 14
              case "K" => 13
              case "Q" => 12
              case "J" => 11
              case "T" => 10
              case _ => (card+"").toInt
   
    def getValue(ht:HandType) : Int = 
      return ht match
              case HandType.FiveKind => 7
              case HandType.FourKind => 6
              case HandType.FullHouse => 5
              case HandType.ThreeKind => 4
              case HandType.TwoPair => 3 
              case HandType.OnePair => 2
              case HandType.HighCard => 1 

    def isBetterThan(h:Hand) : Boolean =
      if (getValue(typeH) != getValue(h.typeH)) {
        // compare type
        getValue(typeH) > getValue(h.typeH)
        
      } else {
        // compare char
        if (getValue(hand(0)) != getValue(h.hand(0))) {
          getValue(hand(0)) > getValue(h.hand(0)) 
        } else
        if (getValue(hand(1)) != getValue(h.hand(1))) {
          getValue(hand(1)) > getValue(h.hand(1)) 
        } else
        if (getValue(hand(2)) != getValue(h.hand(2))) {
          getValue(hand(2)) > getValue(h.hand(2)) 
        } else
        if (getValue(hand(3)) != getValue(h.hand(3))) {
          getValue(hand(3)) > getValue(h.hand(3)) 
        } else {
          getValue(hand(4)) > getValue(h.hand(4)) 
        }
      }
    override def toString() : String = 
      "Hand(%s)".format(hand)


  def calcResult(hands:IndexedSeq[Input],result:Long) : Long =
    if (hands.length == 0) {
      result
    } else {
      calcResult(hands.tail,result+(hands.head.v * hands.length))
    }

  def runStep1(p: os.Path, f: String, debug:Boolean) : Long =
    val data : IndexedSeq[String] = os.read.lines(p / f)
    val hands : IndexedSeq[Input] = data.map(x => x.split(" ")).map(x => Input(Hand(x(0)),x(1).toInt))
    val sortedHands : IndexedSeq[Input] = hands.sortWith(_.isBetterThan(_))
    if (debug) {
      println(data)
      println(hands.head)
      println(sortedHands)
    }
    return calcResult(sortedHands,0)

  def runStep2(p: os.Path, f: String, debug:Boolean) : Int = 
    return 0


@main def runDay07() = 
  val day = Day07()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true))
  println("Step1 : Full")
  println(day.runStep1(Utils.dataFullPath,day.dataFileFull,true))
  // println("Step2 : Sample")
  // println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step2 : Full")
  // println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
