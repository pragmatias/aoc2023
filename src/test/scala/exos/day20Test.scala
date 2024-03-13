import fr.pragmatias.exos.Day20
import fr.pragmatias.tools.Utils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class Day20Test extends AnyFlatSpec with Matchers {
  
  val day = Day20()
  

  it should "Check Step n°1 with Sample Data 1.1" in {
    val res = day.runStep1(Utils.dataSamplePath,day.dataFileS11,false)
    res shouldEqual 32000000L
  }

  it should "Check Step n°1 with Sample Data 1.2" in {
    val res = day.runStep1(Utils.dataSamplePath,day.dataFileS12,false)
    res shouldEqual 11687500L
  }

  it should "Check Step n°1 with Full Data" in {
    val res = day.runStep1(Utils.dataFullPath,day.dataFileFull,false)
    res shouldEqual 899848294L
  }
  
  it should "Check Step n°2 with Full Data" in {
    val res = day.runStep2(Utils.dataFullPath,day.dataFileFull,false)
    res shouldEqual 247454898168563L
  }

}
