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

        val kitchens = List.tabulate(1)(n=> n+2)
        val ginis = List.tabulate(2)(i=> i * 0.05)

        val combinatory =
          kitchens.flatMap: k=>
            ginis.map: g=>
              k-> g

//        implicit class StringWrap(s: String):
//          def pretify = s.slice(6,14).replace(".","").toInt

        val worlds = combinatory.foreach: (k,g)=>
          val lands = usecase.GenerateSyntheticParcel.generate(k, g, 300, 0.01.toFloat, 77L, null).toArray.map: sp=>
            val syntheticParcel = sp.asInstanceOf[SyntheticParcel]
            Data.ParcelJson(syntheticParcel.id.toInt, syntheticParcel.ownerID, "%.2f".format(syntheticParcel.area), syntheticParcel.regionID, "%.2f".format(syntheticParcel.distanceToCenter))
          println("NB parcels " + lands.size)
          val jsonText = lands.asJson.noSpaces
          val outPath = s"$path".toFile

          outPath.toJava.mkdir
          s"$outPath/k${k}g${"%.2f".format(g)}.json".toFile.overwrite(jsonText)

      case None=> println("Please, specify an output path")

