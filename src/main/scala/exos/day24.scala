package fr.pragmatias.exos

import fr.pragmatias.tools.Utils
import scala.annotation.tailrec

class Day24() :

  val dataFileS1 = "day24_step1.txt"
  val dataFileFull = "day24.txt"
  val lim = 1000

  case class Pos(x:Double,y:Double) :
    def isInArea(minArea:Long,maxArea:Long):Boolean = 
      return x >= minArea && y >= minArea && x <= maxArea && y <= maxArea
    def isOnHail(h : Hail2D) : Boolean = 
      return h.getMicroSeconds(this) > 0
      
  case class Vel(x:Double,y:Double) 

  case class Hail3D(px:Double,py:Double,pz:Double,vx:Double,vy:Double,vz:Double):
    def getHailXY() : Hail2D = Hail2D(Pos(px,py),Vel(vx,vy))
    def getHailXZ() : Hail2D = Hail2D(Pos(px,pz),Vel(vx,vz))
  case class Hail2D(p:Pos,v:Vel) :
    def getNextHail : Option[Hail2D] = 
      var px = p.x 
      var py = p.y 
      var vx = v.x 
      var vy = v.y
      if v.y == (lim * -1) then 
        if v.x == (lim * -1) then 
          if p.y == (lim * -1) then 
            if p.x == (lim * -1) then 
              return None
            else
              // println("getNextHail old (x-1) -> "+this)
              px -= 1 
              py = lim 
              vx = lim
              vy = lim
          else 
            // println("getNextHail old (y-1) -> "+this)
            py -= 1 
            vx = lim 
            vy = lim 
        else 
          vx -= 1 
          vy = lim 
      else 
        vy -= 1 
      return Some(Hail2D(Pos(px,py),Vel(vx,vy)))

    def getMicroSeconds(p1:Pos) : Double = 
      return if (v.x != 0) then (p1.y - p.y) / v.y else (p1.x - p.x) / v.x
    def getCrossPos(newH : Hail2D) : Option[Pos] = 
      /* equation : 
          slope m = (vy/vx)
          equa y-p1.y = m1 * (x-p1.x)
          equa y-p2.y = m2 * (x-p2.x)
          y = m1 * (x-p1.x) + p1.y = m2 * (x-p2.x) + p2.y 
          m1*x - m1*p1.x + p1.y = m2*x - m2*p2.x + p2.y 
          m1*x - m2*x = m1*p1.x - m2*p2.x + p2.y - p1.y 
          x*(m1-m2) = ...
          x = (m1*p1.x - m2*p2.x + p2.y - p1.y) / (m1-m2)
      */
      val m1 = if (this.v.x != 0) then (this.v.y / this.v.x) else this.v.y
      val m2 = if (newH.v.x != 0) then (newH.v.y / newH.v.x) else newH.v.y
      if (m1 == m2) then  
        // println("1 : %s - 2: %s - m : (%s,%s) - None".format(this.p,newH.p,m1,m2))
        None
      else 
        val p1 = this.p 
        val p2 = newH.p 
        val x = ((m1 * p1.x) - (m2 * p2.x) + p2.y - p1.y) / ( m1 - m2 )
        val y = (m1 * (x-p1.x)) + p1.y
        // println("1 : %s - 2: %s - m : (%s,%s) - Pos : (%s,%s)".format(this,newH,m1,m2,x,y))
        val resP = Pos(x,y)
        if (resP.isOnHail(this) && resP.isOnHail(newH)) then 
          return Some(resP)
        else 
          return None


  def getListCrossingHailInArea(listHail:List[Hail2D],areaMin:Long,areaMax:Long): Long = 

    @tailrec
    def getListCrossingHailInArea(lh : List[Hail2D], lr : List[Pos]) : List[Pos] =
      if lh.length <= 1 then
        return lr 
      else 
        val nh = lh.head 
        val nlp = lh.tail.map(h => h.getCrossPos(nh)).filter(_.nonEmpty).map(h => h.get)
        return getListCrossingHailInArea(lh.tail,nlp:::lr)

    val res = getListCrossingHailInArea(listHail,List()).filter(p => p.isInArea(areaMin,areaMax))
    return res.length


  //NOT WORKING AT ALL ... PART2 NOT DONE YET
  def tryHail(lh : List[Hail3D]) : Hail2D =
    val startHailXY : Hail2D = Hail2D(Pos(lim,lim),Vel(lim,lim))
    val listHailXY : List[Hail2D] = lh.take(3).map(_.getHailXY())

    @tailrec
    def tryHailRec(start:Option[Hail2D],lh2d:List[Hail2D],res:Int) : Option[Hail2D] =
      if start.isEmpty then 
        return None
      else if (lh2d.length == res) then 
        return start
      else 
        val st = start.get 
        val nlres = lh2d.map(h => (h,st.getCrossPos(h))).filter(h => h._2.nonEmpty)
                        .filter(h => st.getMicroSeconds(h._2.get) == h._1.getMicroSeconds(h._2.get)).length
        // println("tryHailRec - Start : %s - nlres : %s".format(st,nlres))
        return tryHailRec(st.getNextHail,lh2d,nlres)
    

    val res = tryHailRec(Some(startHailXY),listHailXY,0)
    return res match
      case Some(r) => return r
      case _ => return startHailXY
    

      




  def extractHail3DFromInput(data:List[String]) : List[Hail3D] =
     return data.map(s => s match
      // case regHail(px,py,pz,vx,vy,vz) => Hail(Pos(px.toLong,py.toLong,pz.toLong),Vel(vx.toInt,vy.toInt,vz.toInt))
      case s"$px, $py, $pz @ $vx, $vy, $vz" => Hail3D(px.trim.toLong,py.trim.toLong,pz.trim.toLong,vx.trim.toInt,vy.trim.toInt,vz.trim.toInt)
     )

  def runStep1(p: os.Path, f: String, debug:Boolean, minArea : Long, maxArea: Long) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val listHail : List[Hail2D] = extractHail3DFromInput(data.toList).map(h => h.getHailXY())
    val res : Long = getListCrossingHailInArea(listHail,minArea,maxArea)
    if debug then 
      listHail.foreach(println)
    return res

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val listHail : List[Hail3D] = extractHail3DFromInput(data.toList)
    val res = tryHail(listHail)
    if debug then 
      listHail.foreach(println)
      println("Temp Res = "+res)
    return 0


@main def runDay24() = 
  val day = Day24()
  // println("Step1 : Sample")
  // println(day.runStep1(Utils.dataSamplePath,day.dataFileS1,true,7,27))
  // println("Step1 : Full")
  // println(day.runStep1(Utils.dataFullPath,day.dataFileFull,false,200000000000000L,400000000000000L))
  println("Step2 : Sample")
  println(day.runStep2(Utils.dataSamplePath,day.dataFileS1,true))
  // println("Step2 : Full")
  // println(day.runStep2(Utils.dataFullPath,day.dataFileFull,false))
  
