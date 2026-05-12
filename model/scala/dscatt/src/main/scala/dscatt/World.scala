package dscatt

import Parcel.*
import Croping.*
import Kitchen.{KitchenID, kitchen}
import better.files.File
import io.circe.Decoder.*
import io.circe.*
import io.circe.parser.*
import io.circe.generic.semiauto.*
import shared.Data
import better.files.*

import java.io.{File as JFile, *}
import better.files.{File as ScalaFile, *}
import dscatt.Croping.Crop.*
import dscatt.RotationCycle.*

import java.text.DecimalFormat
import scala.annotation.tailrec
import scala.io.Source

object World {

  def buildWorldGeometry(kitchens: Array[Kitchen],
                         lands: java.io.File,
                         data: Data
                        ): World =

    val resource: File = File(lands.getAbsolutePath)
    val content = resource.contentAsString

    implicit val parcelJsonDecoder: Decoder[Data.ParcelJson] = deriveDecoder[Data.ParcelJson]
    decode[Array[Data.ParcelJson]](content) match
      case Right(ps) =>
        val kitchensMap = kitchens.groupBy(_.id)
        val parcels = ps map : p =>
          val area = p.a.replace(",", ".").toDouble * data.AREA_FACTOR
          Parcel(
            id = p.id.toString,
            ownerID = p.oID,
            farmerID = p.oID,
            crop = intToCrop(p.r, kitchensMap(p.oID).head.rotationCycle),
            initiallyPlannedCrop = None,
            area = area,
            faidherbiaTreesByHa = kitchensMap(p.oID).head.nbFaidherbiaByHa,
            Array()
          )

        //Assign NotAssignedYet parcels
        val profileIDs = kitchens.groupBy(_.profileID)

        val reassignedParcels = profileIDs.toSeq.flatMap: (pID, ks) =>
          val parcelsForK = parcelsForKitchenProfile(parcels, ks, pID)
          ks.head.rotationCycle match {
            case MilletFallow => reassignCropsInParcels(parcelsForK, MilletFallow, MilletFallow)
            case MilletPeanut => reassignCropsInParcels(parcelsForK, MilletPeanut, MilletPeanut)
            case _ => parcelsForK
          }
        .toArray

        World(reassignedParcels, kitchens.size)
      case Left(f) =>
        World(Array(), 0)


  def reassignCropsInParcels(parcels: Array[Parcel], previousRotationCycle: RotationCycle, newRotationCycle: RotationCycle) =

    def reassign(parcels: Array[Parcel], ratioObjective: Double, from: Crop, to: Crop): Array[Parcel] =

      val (fromParcels, otherParcels) = parcels.partition(_.crop == from)
      val fromArea = fromParcels.map(_.area).sum

      @tailrec
      def reassignWith(currentParcels: Array[Parcel], selectedParcels: Array[Parcel]): Array[Parcel] =
        val actualRatio = selectedParcels.map(_.area).sum / fromArea
        if (actualRatio >= ratioObjective) || currentParcels.isEmpty
        then selectedParcels.map(_.copy(crop = to)) ++ currentParcels
        else reassignWith(currentParcels.tail, selectedParcels :+ currentParcels.head)

      reassignWith(fromParcels, Array()) ++ otherParcels

    def switch(ps: Array[Parcel], from: Crop, to: Crop): Array[Parcel] =
      ps.map: p =>
        p.copy(crop =
          if p.crop == from
          then to
          else p.crop
        )

    def switchBoth(ps: Array[Parcel], from1: Crop, to1: Crop, from2: Crop, to2: Crop): Array[Parcel] =
      ps.map: p =>
        p.copy(crop =
          if p.crop == from1
          then to1
          else if p.crop == from2
          then to2
          else p.crop
        )

    previousRotationCycle match
      case MilletOnly =>
        newRotationCycle match
          case MilletPeanut => reassign(parcels, 0.5, Millet, Peanut)
          case MilletFallow => reassign(parcels, 0.5, Millet, Fallow)
          case FallowMilletPeanut => reassign(reassign(parcels, 0.33, Millet, Peanut), 0.5, Millet, Fallow)
          case MilletOnly => parcels
      case MilletFallow =>
        newRotationCycle match
          case MilletPeanut => switchBoth(parcels, Millet, Millet, Fallow, Peanut) // To get Millet --> Peanut and Fallow --> Millet after rotation
          case MilletOnly => parcels.map(_.copy(crop = Millet))
          case FallowMilletPeanut => reassign(reassign(parcels, 0.33, Millet, Peanut), 0.33, Fallow, Peanut)
          case MilletFallow => reassign(reassign(parcels, 0.5, NotAssignedYet, Millet), 1.0, NotAssignedYet, Fallow)
      case MilletPeanut =>
        newRotationCycle match
          case MilletFallow => switchBoth(parcels, Millet, Fallow, Peanut, Millet) // To get Millet --> Millet and Peanut --> Fallow after rotation
          case MilletOnly => parcels.map(_.copy(crop = Millet))
          case FallowMilletPeanut => reassign(reassign(parcels, 0.33, Millet, Fallow), 0.33, Peanut, Fallow)
          case MilletPeanut => reassign(reassign(parcels, 0.5, NotAssignedYet, Millet), 1.0, NotAssignedYet, Peanut)
      case FallowMilletPeanut =>
        newRotationCycle match
          case MilletPeanut => switch(reassign(parcels, 0.5, Fallow, Millet), Fallow, Peanut)
          case MilletFallow => reassign(reassign(parcels, 0.5, Peanut, Millet), 1.0, Peanut, Fallow)
          case MilletOnly => parcels.map(_.copy(crop = Millet))
          case FallowMilletPeanut => parcels


  def display(world: World): Unit = {
    world.parcels.foreach { p =>
      println("ID              :" + p.id)
      println("KITCHEN         :" + p.ownerID)
      println("CROP            :" + p.crop)
      println("AREA            :" + p.area + "\n")
    }
  }

  //  private def zoneParcels(world: World, cropZone: CropZone) = world.parcels.filter {
  //    _.cropZone == cropZone
  //  }

  def fullArea(world: World) = world.parcels.map {
    _.area
  }.sum

  //  def zoneOneParcels(world: World) = zoneParcels(world, One)
  //
  //  def zoneTwoParcels(world: World) = zoneParcels(world, Two)
  //
  //  def zoneThreeParcels(world: World) = zoneParcels(world, Three)

  def printCropRatios(world: World) =
    println("MIL: " + milParcels(world.parcels).size +
      "| PEANUT: " + peanutParcels(world.parcels).size +
      "| FALLOW: " + fallowParcels(world.parcels).size +
      "| NA: " + notAssignedYetParcels(world.parcels).size //+
      //" -- " + world.parcels.groupBy(_.ownerID).map(x=> x._1-> x._2.map(_.area).sum)
    )

  def parcelsForKitchen(world: World, kitchen: Kitchen): Array[Parcel] = parcelsForKitchen(world.parcels, kitchen)

  def parcelsForKitchen(parcels: Array[Parcel], kitchen: Kitchen): Array[Parcel] = parcels.filter(_.ownerID == kitchen.id)

  def parcelsForKitchenProfile(parcels: Array[Parcel], kitchens: Array[Kitchen], profileID: KitchenProfileID) =
    val selectedKitchens = kitchens.filter(_.profileID == profileID)
    selectedKitchens.flatMap(k=> parcelsForKitchen(parcels, k))


  def ownedAreaForKitchen(world: World, kitchen: Kitchen) = parcelsForKitchen(world, kitchen).map(_.area).sum

  def farmedParcelsForKitchen(parcels: Array[Parcel], kitchen: Kitchen): Array[Parcel] = farmedParcelsForKitchenID(parcels, kitchen.id)

  def farmedParcelsForKitchenID(parcels: Array[Parcel], kitchenID: KitchenID): Array[Parcel] = parcels.filter(_.farmerID == kitchenID)

  def farmedParcelsForKitchen(world: World, kitchen: Kitchen): Array[Parcel] = farmedParcelsForKitchen(world.parcels, kitchen)

  def parcelsInCultureForKitchen(world: World, kitchen: Kitchen): Array[Parcel] = parcelsInCultureForKitchen(world.parcels, kitchen)

  def parcelsInCultureForKitchen(parcels: Array[Parcel], kitchen: Kitchen): Array[Parcel] = parcelsInCultureForKitchenID(parcels, kitchen.id)

  def parcelsInCultureForKitchenID(parcels: Array[Parcel], kitchenID: KitchenID): Array[Parcel] = parcels.filter { p => p.farmerID == kitchenID && Parcel.isCultivated(p) }

  def cultivatedParcels(parcels: Array[Parcel]): Array[Parcel] = parcels.filter(Parcel.isCultivated(_))

  def milParcels(parcels: Array[Parcel]) = parcels.filter(_.crop == Millet)

  def peanutParcels(parcels: Array[Parcel]) = parcels.filter(_.crop == Peanut)

  def fallowParcels(parcels: Array[Parcel]): Array[Parcel] = parcels.filter(_.crop == Fallow)

  def notAssignedYetParcels(parcels: Array[Parcel]): Array[Parcel] = parcels.filter(_.crop == NotAssignedYet)

  def fallowParcels(world: World): Array[Parcel] = fallowParcels(world.parcels)

  def fallowParcelsForKitchen(world: World, kitchen: Kitchen) = parcelsForKitchen(world, kitchen).filter(_.crop == Fallow)

}

case class World(parcels: Array[Parcel], highestKitckenID: KitchenID)
