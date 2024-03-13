import fr.pragmatias.exos.Day11
import fr.pragmatias.tools.Utils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class Day11Test extends AnyFlatSpec with Matchers {
  
  val day = Day11()
  

  it should "Check Step n°1 with Sample Data" in {
    val res = day.runStep1(Utils.dataSamplePath,day.dataFileS1,false)
    res shouldEqual 374
  }

  it should "Check Step n°1 with Full Data" in {
    val res = day.runStep1(Utils.dataFullPath,day.dataFileFull,false)
    res shouldEqual 10313550
  }
  
  it should "Check Step n°2 with Sample Data (with 10)" in {
    val res = day.runStep2(Utils.dataSamplePath,day.dataFileS1,false,10L)
    res shouldEqual 1030
  }
  
  it should "Check Step n°2 with Sample Data (with 100)" in {
    val res = day.runStep2(Utils.dataSamplePath,day.dataFileS1,false,100L)
    res shouldEqual 8410
  }
  
  it should "Check Step n°2 with Full Data (with 1000000)" in {
    val res = day.runStep2(Utils.dataFullPath,day.dataFileFull,false,1000000L)
    res shouldEqual 611998089572L
  }

}
