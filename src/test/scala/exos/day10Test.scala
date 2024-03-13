import fr.pragmatias.exos.Day10
import fr.pragmatias.tools.Utils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class Day10Test extends AnyFlatSpec with Matchers {
  
  val day = Day10()
  

  it should "Check Step n°1 with Sample Data 1" in {
    val res = day.runStep1(Utils.dataSamplePath,day.dataFileS1,false)
    res shouldEqual 8
  }

  it should "Check Step n°1 with Full Data" in {
    val res = day.runStep1(Utils.dataFullPath,day.dataFileFull,false)
    res shouldEqual 6773
  }
  
  it should "Check Step n°2 with Sample Data 2.1" in {
    val res = day.runStep2(Utils.dataSamplePath,day.dataFileS21,false)
    res shouldEqual 4
  }
  
  it should "Check Step n°2 with Sample Data 2.2" in {
    val res = day.runStep2(Utils.dataSamplePath,day.dataFileS22,false)
    res shouldEqual 8
  }
  
  it should "Check Step n°2 with Sample Data 2.3" in {
    val res = day.runStep2(Utils.dataSamplePath,day.dataFileS23,false)
    res shouldEqual 10
  }
  
  it should "Check Step n°2 with Full Data" in {
    val res = day.runStep2(Utils.dataFullPath,day.dataFileFull,false)
    res shouldEqual 493
  }

}
