package dscatt

import Parcel.*
import Croping.*
import Kitchen.KitchenID
import org.apache.commons.math3.random.MersenneTwister
import io.circe.Decoder.*
import io.circe.*
import io.circe.parser.*
import io.circe.generic.semiauto.*
import shared.Data

import java.text.DecimalFormat
import scala.io.Source

object World {

  def buildWorldGeometry(kitchens: Seq[Kitchen],
                         giniIndex: Double
                        )(using mT: MersenneTwister): World =

    val parcelPath = s"json/k${kitchens.size}g${String.format(java.util.Locale.FRANCE, "%.2f", giniIndex)}.json"
    val resource = scala.io.Source.fromResource(parcelPath, World.getClass.getClassLoader).getLines().mkString("\n")

    implicit val parcelJsonDecoder: Decoder[Data.ParcelJson] = deriveDecoder[Data.ParcelJson]
    decode[List[Data.ParcelJson]](resource) match
      case Right(ps) =>
        val kitchensMap = kitchens.groupBy(_.id)
        val parcels = ps map: p=>
            val area = p.a.replace(",",".").toDouble * Constants.AREA_FACTOR
            Parcel(
              id = p.id.toString,
              ownerID = p.oID,
              farmerID = p.oID,
              crop = Fallow,
              cropZone = intToCropZone(p.r, kitchensMap(p.oID).head.rotationCycle, mT.nextDouble() > 0.5),
              area = area,
              faidherbiaTrees = kitchensMap(p.oID).head.nbFaidherbia * area,
              Seq()
            )
        World(parcels, kitchens.size)
      case Left(f) =>
        World(Seq(), 0)
  

  def display(world: World): Unit = {
    world.parcels.foreach { p =>
      println("ID              :" + p.id)
      println("KITCHEN         :" + p.ownerID)
      println("CROP ZONE       :" + p.cropZone)
      println("AREA            :" + p.area + "\n")
    }
  }

  private def zoneParcels(world: World, cropZone: CropZone) = world.parcels.filter {
    _.cropZone == cropZone
  }

  def fullArea(world: World) = world.parcels.map {
    _.area
  }.sum

  def zoneOneParcels(world: World) = zoneParcels(world, One)

  def zoneTwoParcels(world: World) = zoneParcels(world, Two)

  def zoneThreeParcels(world: World) = zoneParcels(world, Three)

  def parcelsForKitchen(world: World, kitchen: Kitchen) = world.parcels.filter(_.ownerID == kitchen.id)

  def farmedParcelsForKitchen(parcels: Seq[Parcel], kitchen: Kitchen): Seq[Parcel] = farmedParcelsForKitchenID(parcels,kitchen.id)
  
  def farmedParcelsForKitchenID(parcels: Seq[Parcel], kitchenID: KitchenID): Seq[Parcel] = parcels.filter(_.farmerID == kitchenID)

  def farmedParcelsForKitchen(world: World, kitchen: Kitchen): Seq[Parcel] = farmedParcelsForKitchen(world.parcels, kitchen)

  def ownedParcelsForKitchen(world: World, kitchen: Kitchen) = world.parcels.filter(_.ownerID == kitchen.id)

  def parcelsInCultureForKitchen(world: World, kitchen: Kitchen): Seq[Parcel] = parcelsInCultureForKitchen(world.parcels, kitchen)

  def parcelsInCultureForKitchen(parcels: Seq[Parcel], kitchen: Kitchen): Seq[Parcel] = parcelsInCultureForKitchenID(parcels, kitchen.id)

  def parcelsInCultureForKitchenID(parcels: Seq[Parcel], kitchenID: KitchenID): Seq[Parcel] = parcels.filter { p => p.farmerID == kitchenID && Parcel.isCultivated(p) }

  def cultivatedParcels(parcels: Seq[Parcel]): Seq[Parcel] = parcels.filter(Parcel.isCultivated(_))

  def milParcels(parcels: Seq[Parcel]) = parcels.filter(_.crop == Mil)

  def peanutParcels(parcels: Seq[Parcel]) = parcels.filter(_.crop == Peanut)

  def fallowParcels(world: World) = world.parcels.filter(_.crop == Fallow)

  def fallowParcelsForKitchen(world: World, kitchen: Kitchen) = parcelsForKitchen(world, kitchen).filter(_.crop == Fallow)
  
}

case class World(parcels: Seq[Parcel], highestKitckenID: KitchenID)
