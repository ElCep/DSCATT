package parcelgenerator

import fr.ign.artiscales.pm.parcel.SyntheticParcel
import fr.ign.artiscales.pm.{parcel, usecase}

import better.files._
import io.circe.generic.auto.*
import io.circe.syntax.*
import scala.jdk.CollectionConverters.*
import shared.Data

object App:

  def main(args: Array[String]) =
    val arguments = args.lift
    val outputPath = arguments(0) match
      case Some(path: String)=>

        val kitchens = List.tabulate(20)(n => n + 35)
        val ginis = List.tabulate(10)(i=> 0.05 * (i + 1))

        val combinatory =
          kitchens.flatMap: k=>
            ginis.map: g=>
              k-> g

        val worlds = combinatory.foreach: (k,g)=>
          println("file name " + k + " " + g)
          println(s"${"%.2f".format(g)}")
          val fileName = s"k${k}g${"%.2f".format(g)}"
          val outPath = s"$path".toFile
          val synthParcels = usecase.GenerateSyntheticParcel.generate(k, g, 350, 0.01.toFloat, 77L, null).toArray.map: sp=>
              sp.asInstanceOf[SyntheticParcel]

          outPath.toJava.mkdir
          val gpkgDir = File(s"${outPath.path}/gpkg/")
          val jsonDir = File(s"${outPath.path}/json/")
          gpkgDir.toJava.mkdir
          jsonDir.toJava.mkdir

          val lands = synthParcels.map{syntheticParcel=> Data.ParcelJson(syntheticParcel.id.toInt, syntheticParcel.ownerID, "%.2f".format(syntheticParcel.area), syntheticParcel.regionID)}
          SyntheticParcel.`export`(synthParcels.toList.asJava, (gpkgDir/s"${fileName}.gpkg").toJava)

          println("NB parcels " + lands.size)
          val jsonText = lands.asJson.spaces2

          (jsonDir/s"${fileName}.json").overwrite(jsonText)

      case None=> println("Please, specify an output path")

