package dscatt

import Parcel.*
import dscatt.Croping.*
import dscatt.Kitchen.KitchenID
import org.apache.commons.math3.random.MersenneTwister
import io.circe.Decoder.*
import io.circe.*
import io.circe.parser.*
import io.circe.generic.semiauto.*
import shared.Data

import scala.io.Source

object World {

  def buildWorldGeometry(kitchens: Seq[Kitchen],
                         giniIndex: Double,
                         giniTolerance: Double = 0.01,
                         maximumNumberOfParcels: Int = 200,
                         seed: Long,
                         geometryImagePath: Option[String] = None
                        )(using mT: MersenneTwister): World =

    val parcelPath = s"generatedParcels/k${kitchens.size}g${"%.2f".format(giniIndex)}.json"
    val resource = scala.io.Source.fromResource(parcelPath).getLines().mkString("\n")

    implicit val parcelJsonDecoder: Decoder[Data.ParcelJson] = deriveDecoder[Data.ParcelJson]
    decode[List[Data.ParcelJson]](resource) match
      case Right(ps) =>
        val kitchensMap = kitchens.groupBy(_.id)
        val parcels = ps map: p=>
            Parcel(
              id = p.id.toString,
              ownerID = p.oID,
              farmerID = p.oID,
              crop = NotAssigned,
              cropZone = intToCropZone(p.r, kitchensMap(p.oID).head.rotationCycle, mT.nextDouble() > 0.5),
              area = p.a.replace(",",".").toDouble * Constants.AREA_FACTOR,
              distanceToVillage = p.dC.replace(",",".").toDouble,
              faidherbiaTrees = 0,
              Seq()
            )
        World(parcels, kitchens.size)
      case Left(f) =>
        println("LEFT")
        println(f)
        World(Seq(), 0)
  

  def display(world: World): Unit = {
    world.parcels.foreach { p =>
      println("ID              :" + p.id)
      println("KITCHEN         :" + p.ownerID)
      println("CROP ZONE       :" + p.cropZone)
      println("DIST TO VILLAGE :" + p.distanceToVillage)
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

  def farmedParcelsForKitchen(parcels: Seq[Parcel], kitchen: Kitchen): Seq[Parcel] = parcels.filter(_.farmerID == kitchen.id)

  def farmedParcelsForKitchen(world: World, kitchen: Kitchen): Seq[Parcel] = farmedParcelsForKitchen(world.parcels, kitchen)

  def assignedParcelsForKitchen(world: World, kitchen: Kitchen): Seq[Parcel] = farmedParcelsForKitchen(world, kitchen).filter { p => Parcel.isAssigned(p) }

  def ownedParcelsForKitchen(world: World, kitchen: Kitchen) = world.parcels.filter(_.ownerID == kitchen.id)

  def parcelsInCultureForKitchen(world: World, kitchen: Kitchen): Seq[Parcel] = parcelsInCultureForKitchen(world.parcels, kitchen)

  def parcelsInCultureForKitchen(parcels: Seq[Parcel], kitchen: Kitchen): Seq[Parcel] = parcels.filter { p => p.farmerID == kitchen.id && Parcel.isCultivated(p) }

  def notAssignedParcels(world: World) = world.parcels.filter(_.crop == NotAssigned)

  def assignedParcels(world: World) = world.parcels.filter(_.crop != NotAssigned)

  def milParcels(world: World) = world.parcels.filter(_.crop == Mil)

  def peanutParcels(world: World) = world.parcels.filter(_.crop == Peanut)

  def fallowParcels(world: World) = world.parcels.filter(_.crop == Fallow)

  def fallowParcelsForKitchen(world: World, kitchen: Kitchen) = parcelsForKitchen(world, kitchen).filter(_.crop == Fallow)

  def setFallowLast(parcels: Seq[Parcel]): Seq[Parcel] = {
    val (fallow, others) = parcels.partition(_.crop == Fallow)
    others ++ fallow
  }
}

case class World(parcels: Seq[Parcel], highestKitckenID: KitchenID)
