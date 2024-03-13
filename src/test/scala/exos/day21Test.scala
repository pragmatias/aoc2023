import fr.pragmatias.exos.Day21
import fr.pragmatias.tools.Utils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class Day21Test extends AnyFlatSpec with Matchers {
  
  val day = Day21()
  

  it should "Check Step n°1 with Sample Data" in {
    val res = day.runStep1(Utils.dataSamplePath,day.dataFileS1,false,6)
    res shouldEqual 16
  }

  it should "Check Step n°1 with Full Data" in {
    val res = day.runStep1(Utils.dataFullPath,day.dataFileFull,false,64)
    res shouldEqual 3758
  }
  
  // it should "Check Step n°2 with Sample Data" in {
  //   val res = day.runStep2(Utils.dataSamplePath,day.dataFileS1,false)
  //   res shouldEqual ???
  // }
  // 
  // it should "Check Step n°2 with Full Data" in {
  //   val res = day.runStep2(Utils.dataFullPath,day.dataFileFull,false)
  //   res shouldEqual ???
  // }

}
