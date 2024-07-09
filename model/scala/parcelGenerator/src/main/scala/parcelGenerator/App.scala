package parcelgenerator

import fr.ign.artiscales.pm.parcel.SyntheticParcel
import fr.ign.artiscales.pm.{parcel, usecase}

import better.files._
import io.circe.generic.auto.*
import io.circe.syntax.*
import scala.jdk.CollectionConverters.*
import shared.Data

object App:

  @main def run() =
    replicateLands("/tmp/lands", 50)
   // buildLands("/tmp/lands", 7L, 21, List.tabulate(10)(i => 0.05 * (i + 1)))

  def replicateLands(dirPath: String, quantity: Int) =
      for i <- 1 to quantity
      do
        println("Build  " + i.toLong)
        buildLand(dirPath, i.toLong, 22, 0.2)


  def buildLands(outputPath: String, seed: Long, nbKitchens: Int, ginis: Seq[Double]) =
    val kitchens = List.tabulate(nbKitchens)(n => n + 15)

    val combinatory =
      kitchens.flatMap: k =>
        ginis.map: g =>
          k -> g

    val worlds = combinatory.foreach: (k, g) =>
      println("file name " + k + " " + g)
      println(s"${"%.2f".format(g)}")
      buildLand(outputPath, seed, k, g)


  private def buildLand(outputPath: String, seed: Long, nbKitchens: Int, gini: Double) =

    val fileName = s"s${seed.toString}k${nbKitchens}g${"%.2f".format(gini)}"
    val outFile = s"$outputPath".toFile

    val synthParcels = usecase.GenerateSyntheticParcel.generate(nbKitchens, gini, 316, 0.01.toFloat, seed, null).toArray.map: sp =>
      sp.asInstanceOf[SyntheticParcel]

    outFile.toJava.mkdir
    val gpkgDir = File(s"$outputPath/gpkg/")
    val jsonDir = File(s"$outputPath/json/")
    gpkgDir.toJava.mkdir
    jsonDir.toJava.mkdir

    val lands = synthParcels.map { syntheticParcel => Data.ParcelJson(syntheticParcel.id.toInt, syntheticParcel.ownerID, "%.2f".format(syntheticParcel.area), syntheticParcel.regionID) }
    SyntheticParcel.`export`(synthParcels.toList.asJava, (gpkgDir / s"${fileName}.gpkg").toJava)

    println("NB parcels " + lands.size)
    val jsonText = lands.asJson.spaces2

    (jsonDir / s"${fileName}.json").overwrite(jsonText)
