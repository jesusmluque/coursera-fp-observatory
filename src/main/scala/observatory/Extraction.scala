package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.spark.sql.{DataFrame, Dataset}
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._

/**
  * 1st milestone: data extraction
  */
object Extraction {

  import org.apache.spark.sql.SparkSession

  case class TemperaturesRank(year: Int, month: Int, day: Int, latitude: Double, longitude: Double, temp: Double)

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Time Usage")
      .config("spark.master", "local")
      .getOrCreate()

  import spark.implicits._

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    locateTemperaturesDS(year: Year, stationsFile: String, temperaturesFile: String)
      .collect()
      .map { row =>
        (LocalDate.of(row.year, row.month, row.day), Location(row.latitude, row.longitude), row.temp)
    }.toList
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.groupBy { temp =>
      temp._2
    }.map { temps =>
      (temps._1, temps._2.foldLeft(0D) { (acc, t) =>
        (acc + t._3)
      } / temps._2.size)
    }
  }

  def locateTemperaturesDS(year: Year, stationsFile: String, temperaturesFile: String): Dataset[TemperaturesRank] = {
    val rddStations: DataFrame = stationsDF(stationsFile)
    val rddTemps: DataFrame = temperaturesDF(year, temperaturesFile)

    rddStations
      .join(rddTemps,
        (rddStations("stnId") === rddTemps("stnId") || (rddStations("stnId").isNull && rddTemps("stnId").isNull))
          && (rddStations("wbanId") === rddTemps("wbanId") || rddStations("wbanId").isNull && rddTemps("wbanId").isNull)
          && (rddStations("longitude").isNotNull && rddStations("latitude").isNotNull))
      .select(rddStations("latitude"), rddStations("longitude"), rddTemps("month"), rddTemps("day"), rddTemps("temp"))
      .withColumn("year", lit(year))
      .withColumn("temp", ($"temp" - 32D) / 1.8)
      .orderBy($"temp".desc).as[TemperaturesRank]
  }

  private def temperaturesDF(year: Year, temperaturesFile: String): DataFrame = spark
      .read
      .schema(StructType(Array(
        StructField("stnID", StringType),
        StructField("wbanId", StringType),
        StructField("month", IntegerType),
        StructField("day", IntegerType),
        StructField("temp", DoubleType))))
      .csv(fsPath(temperaturesFile))

  private def stationsDF(stationsFile: String): DataFrame = spark
      .read
      .schema(StructType(Array(
        StructField("stnID", StringType),
        StructField("wbanId", StringType),
        StructField("latitude", DoubleType),
        StructField("longitude", DoubleType))))
      .csv(fsPath(stationsFile))
      .persist()


  /** @return The filesystem path of the given resource */
  private def fsPath(resource: String): String = Paths.get(getClass.getResource(resource).toURI).toString

}
