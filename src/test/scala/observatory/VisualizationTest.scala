package observatory


import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTest extends FunSuite with Checkers {

  test("interpolate a value in range 60") {
    assert(Visualization.interpolateColor(List((59D,Color(254,254,254)), (33D,Color(254,254,1))), 55D) ==
      Color(254, 254, 215))

  }
  test("interpolate a value in range 60 corner case 60") {
    assert(Visualization.interpolateColor(List((59D,Color(254,254,254)), (33D,Color(254,254,1))), 60D) ==
      Color(255, 255, 255))

  }
  test("interpolate a value in range 60 corner case 32") {
    assert(Visualization.interpolateColor(List((59D,Color(254,254,254)), (33D,Color(254,254,1))), 32D) ==
      Color(255, 0, 0))

  }
  test("interpolate a value in range 32") {
    assert(Visualization.interpolateColor(List((31D,Color(254,0,0)), (12D,Color(255,255,0))), 20D) ==
      Color(254, 147, 0))

  }
}
