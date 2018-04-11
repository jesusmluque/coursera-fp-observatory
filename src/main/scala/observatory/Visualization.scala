package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    def distance(x: Location, location:Location):Double =
      6371000D * Math.asin(Math.sin(x.lat/180) * Math.sin(location.lat) + Math.cos(x.lat) * Math.cos(location.lat) *
        Math.cos(location.lon - x.lon))
    def weight(distance: Double):Double = {
      1 / Math.pow(distance, 2)
    }
    val distancesAndTemp: Iterable[(Double, Temperature)] = temperatures.map { temp =>
      (distance(temp._1, location), temp._2)
    }
    val minDistanceTemp = distancesAndTemp.minBy(_._1)
    if (minDistanceTemp._1 < 1000D)
      minDistanceTemp._2
    else {
      val numDem = distancesAndTemp.foldLeft((0D, 0D)) { (acc, distTemp) =>
        val w = weight(distTemp._1)
        (acc._1 + w * distTemp._2, acc._2 + w)
      }
      numDem._1 / numDem._2
    }

  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    def getTempRange(temp: Temperature) = {
      temp match {
        case t if t <= 60D && t > 32D => 60
        case t if t <= 32D && t > 12D => 32
        case t if t <= 12D && t > 0D => 12
        case t if t <= 0D && t > -15D => 0
        case t if t <= -15D && t > -27D => -15
        case t if t <= -27D && t > -50D => -27
        case t if t <= -50D && t > -60D => -50
        case t if t <= -60 => -60
      }
    }
    val threshold = Map(
      (60D, Color(255,255,255)),
      (32D, Color(255,0,0)),
      (12D, Color(255,255,0)),
      (0D, Color(0,255,255)),
      (-15D, Color(0,0,255)),
      (-27D, Color(255,0,255)),
      (-50D, Color(33,0,107)),
      (-60D, Color(0,0,0)))

    def findClosed(sortedList:List[(Temperature, Color)], value: Temperature): ((Temperature, Color), Temperature, (Temperature, Color)) = {
      sortedList match {
        case (x :: y :: _) if y._1 > value => (x, value, y)
        case (x :: _) if x._1 > value => ((getTempRange(value), threshold(getTempRange(value))), value, x)
        case (x :: xs) if x._1 < value =>  findClosed(xs, value)
        case (x :: Nil) if x._1 < value => (x, value, (getTempRange(value), threshold(getTempRange(value))))
      }
    }
    val (v0, _, v1) = findClosed(points.toList.sortBy(_._1), value)

    def getNewColorComponent(v0color: Int, v1color: Int) = {
      ((v0color * (v1._1 - value) + v1color * (value - v0._1)) / (v1._1 - v0._1)).round.toInt
    }

    Color(
      getNewColorComponent(v0._2.red, v1._2.red),
      getNewColorComponent(v0._2.green, v1._2.green),
      getNewColorComponent(v0._2.blue, v1._2.blue))
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    ???
  }

}

