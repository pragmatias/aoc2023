import fr.pragmatias.exos.Day19
import fr.pragmatias.tools.Utils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class Day19Test extends AnyFlatSpec with Matchers {
  
  val day = Day19()
  

  it should "Check Step n°1 with Sample Data" in {
    val res = day.runStep1(Utils.dataSamplePath,day.dataFileS1,false)
    res shouldEqual 19114
  }

  it should "Check Step n°1 with Full Data" in {
    val res = day.runStep1(Utils.dataFullPath,day.dataFileFull,false)
    res shouldEqual 532551
  }
  
  it should "Check Step n°2 with Sample Data" in {
    val res = day.runStep2(Utils.dataSamplePath,day.dataFileS1,false)
    res shouldEqual 167409079868000L
  }
  
  it should "Check Step n°2 with Full Data" in {
    val res = day.runStep2(Utils.dataFullPath,day.dataFileFull,false)
    res shouldEqual 134343280273968L
  }

}
