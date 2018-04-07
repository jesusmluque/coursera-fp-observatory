package observatory

import org.scalatest.FunSuite

trait ExtractionTest extends FunSuite {

  test("Extract location temperatures") {
    val temps = Extraction.locateTemperatures(1977, "/stations-test1.csv", "/1977.csv")

    assert(temps.size === 4)
    assert(temps.head._3 == 27.3)
    assert(temps.toArray.apply(3)._1.getDayOfMonth == 6)
  }

  test("Averages") {

    val temps = Extraction.locateTemperatures(1977, "/stations-test1.csv", "/1977.csv")

    val avrg = Extraction.locationYearlyAverageRecords(temps)
    assert(avrg.size === 3)

  }
}