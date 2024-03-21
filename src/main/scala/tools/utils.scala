package fr.pragmatias.tools

object Utils :
  val dataSamplePath: os.Path = os.pwd / "data" / "sample"
  val dataFullPath: os.Path = os.pwd / "data" / "full"


  def getTime[T,A](input:A,debug:Boolean, fun : => T) : T =  {
    val b = System.nanoTime()
    val result = fun
    val a = System.nanoTime()
    if debug then println("Elapsed Time : %s ms - Result : %s - Input : %s".format((a-b)/1000000,result,input))
    return result
  }

  def getTime[T,A](debug:Boolean,fun : => T) : T =  {
    val b = System.nanoTime()
    val result = fun
    val a = System.nanoTime()
    if debug then println("Elapsed Time : %s ms - Result : %s".format((a-b)/1000000,result))
    return result
  }
