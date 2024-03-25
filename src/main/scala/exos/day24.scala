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
    def getMicroSeconds(p1:Pos) : Double = 
      return if (v.x == 0) then (p1.y - p.y) / v.y else (p1.x - p.x) / v.x
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
    
    def deltaV(dvx:Long, dvy:Long): Hail2D = copy(v = Vel(v.x - dvx,v.y - dvy))


  def getListCrossingHailInArea(listHail:List[Hail2D],areaMin:Long,areaMax:Long): List[(Hail2D,Hail2D,Pos)] = 

    @tailrec
    def getListCrossingHailInArea(lh : List[Hail2D], lr : List[(Hail2D,Hail2D,Pos)]) : List[(Hail2D,Hail2D,Pos)] =
      if lh.length <= 1 then
        return lr 
      else 
        val nh = lh.head 
        val nlp : List[(Hail2D,Hail2D,Pos)] = lh.tail.map(h => (h,h.getCrossPos(nh))).filter(_._2.nonEmpty).map(h => (nh,h._1,h._2.get))
        return getListCrossingHailInArea(lh.tail,nlp:::lr)

    val res = getListCrossingHailInArea(listHail,List()).filter(p => p._3.isInArea(areaMin,areaMax))
    return res


  /**** Begin - Bloc fort Part 2 but don't works for Full data ... 
   need to find another idea
  ****/
  def findRockOrigin( hails: List[Hail2D], vx:Long, vy:Long ) : Option[Pos] = 
    val hs = hails.slice(0,3).map(_.deltaV(vx,vy))
    val h0 = hs(0)
    val h1 = hs(1)
    val h2 = hs(2)
    val p1 = h0.getCrossPos(h1)
    val p2 = h0.getCrossPos(h2)
    if p1.nonEmpty && p2.nonEmpty && (p1.get.x == p2.get.x && p1.get.y == p2.get.y) then 
      val time = h0.getMicroSeconds(p1.get)
      Some(Pos(h0.p.x + (h0.v.x * time),h0.p.y + (h0.v.y * time)))
    else 
      None
  end findRockOrigin


  final case class Spiral(
    x: Long, y:Long,
    dx:Long, dy:Long,
    count: Long, limit: Long) : 
    def next : Spiral = 
      if count > 0 then 
        copy(x = x+dx, y= y+dy, count = count-1)
      else if dy == 0 then
        copy(x = x+dx, y = y+dy, dy = dx, dx = -dy, count = limit)
      else 
        copy(x = x+dx, y = y+dy, dy = dx, dx = -dy, count = limit +1, limit = limit+1)
    end next 
  end Spiral

  object Spiral : 
    final val Start = Spiral(0,0,1,0,0,0)


  def SolvePart2(data : List[String]) : Long =
    val listHail : List[Hail3D] = extractHail3DFromInput(data)
    val listHailXY = listHail.map(h => h.getHailXY())
    val listHailXZ = listHail.map(h => h.getHailXZ())

    val Pos(x,y) : Pos = Iterator.iterate(Spiral.Start)(_.next)
      .flatMap(s => findRockOrigin(listHailXY,s.x,s.y)).next()

    val Pos(_,z) : Pos = Iterator.iterate(Spiral.Start)(_.next)
      .flatMap(s => findRockOrigin(listHailXZ,s.x,s.y)).next()

    return (x + y + z).toLong


  /**** End - Bloc fort Part 2 ****/


  def extractHail3DFromInput(data:List[String]) : List[Hail3D] =
     return data.map(s => s match
      // case regHail(px,py,pz,vx,vy,vz) => Hail(Pos(px.toLong,py.toLong,pz.toLong),Vel(vx.toInt,vy.toInt,vz.toInt))
      case s"$px, $py, $pz @ $vx, $vy, $vz" => Hail3D(px.trim.toLong,py.trim.toLong,pz.trim.toLong,vx.trim.toInt,vy.trim.toInt,vz.trim.toInt)
     )

  def runStep1(p: os.Path, f: String, debug:Boolean, minArea : Long, maxArea: Long) : Long =
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val listHail : List[Hail2D] = extractHail3DFromInput(data.toList).map(h => h.getHailXY())
    val res : List[(Hail2D,Hail2D,Pos)] = getListCrossingHailInArea(listHail,minArea,maxArea)
    if debug then 
      listHail.foreach(println)
    return res.length

  def runStep2(p: os.Path, f: String, debug:Boolean) : Long = 
    val data : geny.Generator[String] = os.read.lines.stream(p / f)
    val res = SolvePart2(data.toList)
    return res


@main def runDay24() = 
  val day = Day24()
  // println("Step1 : Sample")
  // println(Utils.getTime(true,day.runStep1(Utils.dataSamplePath,day.dataFileS1,true,7,27)))
  // println("Step1 : Full")
  // println(Utils.getTime(true,day.runStep1(Utils.dataFullPath,day.dataFileFull,false,200000000000000L,400000000000000L)))
  println("Step2 : Sample")
  println(Utils.getTime(true,day.runStep2(Utils.dataSamplePath,day.dataFileS1,true)))
  // println("Step2 : Full")
  // println(Utils.getTime(true,day.runStep2(Utils.dataFullPath,day.dataFileFull,false)))
  
