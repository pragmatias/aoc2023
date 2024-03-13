import fr.pragmatias.exos.Day05
import fr.pragmatias.tools.Utils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class Day05Test extends AnyFlatSpec with Matchers {
  
  val day = Day05()
  

  it should "Check Step n°1 with Sample Data" in {
    val res = day.runStep1(Utils.dataSamplePath,day.dataFileS1,false)
    res shouldEqual 35
  }

  it should "Check Step n°1 with Full Data" in {
    val res = day.runStep1(Utils.dataFullPath,day.dataFileFull,false)
    res shouldEqual 600279879
  }
  
  it should "Check Step n°2 with Sample Data" in {
    val res = day.runStep2(Utils.dataSamplePath,day.dataFileS1,false)
    res shouldEqual 46
  }
  
  it should "Check Step n°2 with Full Data" in {
    val res = day.runStep2(Utils.dataFullPath,day.dataFileFull,false)
    res shouldEqual 20191102
  }

}
