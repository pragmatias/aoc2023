import fr.pragmatias.exos.Day23
import fr.pragmatias.tools.Utils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class Day23Test extends AnyFlatSpec with Matchers {
  
  val day = Day23()
  

  it should "Check Step n°1 with Sample Data" in {
    val res = day.runStep1(Utils.dataSamplePath,day.dataFileS1,false)
    res shouldEqual 94
  }

  it should "Check Step n°1 with Full Data" in {
    val res = day.runStep1(Utils.dataFullPath,day.dataFileFull,false)
    res shouldEqual 2230
  }
  
  it should "Check Step n°2 with Sample Data" in {
    val res = day.runStep2(Utils.dataSamplePath,day.dataFileS1,false)
    res shouldEqual 154
  }
  
  // it should "Check Step n°2 with Full Data" in {
  //   val res = day.runStep2(Utils.dataFullPath,day.dataFileFull,false)
  //   res shouldEqual 6542
  // }

}
