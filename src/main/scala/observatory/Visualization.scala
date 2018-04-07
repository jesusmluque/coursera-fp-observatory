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

    val grouped = points.groupBy[Int] { (temp) =>
      getTempRange(temp._1)
    }
    val valueRange = getTempRange(value)
    val orderedRange = grouped.getOrElse(valueRange, List((valueRange.toDouble, threshold(valueRange.toDouble)))).toList.sortBy(_._1)
    val minMaxRanges = orderedRange.groupBy[String] { t =>
      if (t._1 < value)
        "min"
      else
        "max"
    }
    val v1 = minMaxRanges.getOrElse("max", List((getTempRange(value).toDouble, threshold(getTempRange(value).toDouble)))).minBy(_._1)
    val v0 = minMaxRanges.getOrElse("min", List((getTempRange(value).toDouble, threshold(getTempRange(value).toDouble)))).maxBy(_._1)

    def getNewColorComponent(v0color: Int, v1color: Int) = {
      ((v0color * (v1._1 - value) + v1color * (value - v0._1)) / (v1._1 - v0._1)).toInt
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

