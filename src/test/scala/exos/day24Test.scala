import fr.pragmatias.exos.Day24
import fr.pragmatias.tools.Utils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class Day24Test extends AnyFlatSpec with Matchers {
  
  val day = Day24()
  

  it should "Check Step n°1 with Sample Data" in {
    val res = day.runStep1(Utils.dataSamplePath,day.dataFileS1,false,7,27)
    res shouldEqual 2
  }

  it should "Check Step n°1 with Full Data" in {
    val res = day.runStep1(Utils.dataFullPath,day.dataFileFull,false,200000000000000L,400000000000000L)
    res shouldEqual 2240
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
