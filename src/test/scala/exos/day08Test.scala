import fr.pragmatias.exos.Day08
import fr.pragmatias.tools.Utils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class Day08Test extends AnyFlatSpec with Matchers {
  
  val day = Day08()
  

  it should "Check Step n°1 with Sample Data 1.1" in {
    val res = day.runStep1(Utils.dataSamplePath,day.dataFileS11,false)
    res shouldEqual 2
  }

  it should "Check Step n°1 with Sample Data 1.2" in {
    val res = day.runStep1(Utils.dataSamplePath,day.dataFileS12,false)
    res shouldEqual 6
  }

  it should "Check Step n°1 with Full Data" in {
    val res = day.runStep1(Utils.dataFullPath,day.dataFileFull,false)
    res shouldEqual 16531
  }
  
  it should "Check Step n°2 with Sample Data 2" in {
    val res = day.runStep2(Utils.dataSamplePath,day.dataFileS2,false)
    res shouldEqual 6
  }
  
  it should "Check Step n°2 with Full Data" in {
    val res = day.runStep2(Utils.dataFullPath,day.dataFileFull,false)
    res shouldEqual 24035773251517L
  }

}
