import fr.pragmatias.exos.Day06
import fr.pragmatias.tools.Utils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class Day06Test extends AnyFlatSpec with Matchers {
  
  val day = Day06()
  

  it should "Check Step n°1 with Sample Data" in {
    val res = day.runStep1(Utils.dataSamplePath,day.dataFileS1,false)
    res shouldEqual 288
  }

  it should "Check Step n°1 with Full Data" in {
    val res = day.runStep1(Utils.dataFullPath,day.dataFileFull,false)
    res shouldEqual 4403592
  }
  
  it should "Check Step n°2 with Sample Data" in {
    val res = day.runStep2(Utils.dataSamplePath,day.dataFileS1,false)
    res shouldEqual 71503
  }
  
  it should "Check Step n°2 with Full Data" in {
    val res = day.runStep2(Utils.dataFullPath,day.dataFileFull,false)
    res shouldEqual 38017587
  }

}
