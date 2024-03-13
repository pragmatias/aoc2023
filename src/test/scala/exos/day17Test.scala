import fr.pragmatias.exos.Day17
import fr.pragmatias.tools.Utils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class Day17Test extends AnyFlatSpec with Matchers {
  
  val day = Day17()
  

  it should "Check Step n°1 with Sample Data" in {
    val res = day.runStep1(Utils.dataSamplePath,day.dataFileS1,false)
    res shouldEqual 102
  }

  it should "Check Step n°1 with Full Data" in {
    val res = day.runStep1(Utils.dataFullPath,day.dataFileFull,false)
    res shouldEqual 1256
  }
  
  it should "Check Step n°2 with Sample Data" in {
    val res = day.runStep2(Utils.dataSamplePath,day.dataFileS1,false)
    res shouldEqual 94
  }
  
  it should "Check Step n°2 with Full Data" in {
    val res = day.runStep2(Utils.dataFullPath,day.dataFileFull,false)
    res shouldEqual 1382
  }

}
