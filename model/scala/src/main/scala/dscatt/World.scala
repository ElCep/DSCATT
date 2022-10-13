package dscatt

import fr.ign.artiscales.pm.parcel.SyntheticParcel
import fr.ign.artiscales.pm.{parcel, usecase}

import scala.jdk.CollectionConverters._
import Parcel._
import dscatt.Croping._
import dscatt.Kitchen.KitchenID
import org.apache.commons.math3.random.MersenneTwister

import java.io.File
import scala.annotation.tailrec

object World {

  def buildWorldGeometry(numberOfKitchen: Int,
                         giniIndex: Double,
                         giniTolerance: Double = 0.01,
                         maximumNumberOfParcels: Int = 200,
                         seed: Long,
                         geometryImagePath: Option[String] = None
                        ): World = {

    geometryImagePath.foreach { p =>
      new File(p).mkdirs
    }
    val syntheticParcels = usecase.GenerateSyntheticParcel.generate(
      numberOfKitchen, giniIndex, maximumNumberOfParcels, giniTolerance.toFloat, seed, new java.io.File(geometryImagePath.getOrElse(null)))

    println("all parcels : " + syntheticParcels.toArray.map{p=>
      val pp = p.asInstanceOf[SyntheticParcel]
      pp.area}.sum)

    println("# of parcel INIT " + syntheticParcels.size)
    val parcels = syntheticParcels.toArray.map { sp =>
      val p = sp.asInstanceOf[SyntheticParcel]

      Parcel(
        id = p.id,
        ownerID = p.ownerID,
        farmerID = p.ownerID,
        crop = NotAssigned,
        cropZone = p.regionID,
        area = p.area * Constants.AREA_FACTOR,
        distanceToVillage = p.distanceToCenter,
        neighbours = p.lIdNeighborhood.asScala.toSeq,
        fertility = Constants.INITIAL_FERTILITY_PER_PARCEL
      )
    }

    World(parcels, numberOfKitchen)
  }

  def display(world: World): Unit = {
    world.parcels.foreach { p =>
      println("ID              :" + p.id)
      println("KITCHEN         :" + p.ownerID)
      println("CROP ZONE       :" + p.cropZone)
      println("NEIGHBORHOOD    :" + p.neighbours)
      println("DIST TO VILLAGE :" + p.distanceToVillage)
      println("AREA            :" + p.area + "\n")
    }
  }

  private def zoneParcels(world: World, cropZone: CropZone) = world.parcels.filter {
    _.cropZone == cropZone
  }

  def zoneOneParcels(world: World) = zoneParcels(world, One)

  def zoneTwoParcels(world: World) = zoneParcels(world, Two)

  def zoneThreeParcels(world: World) = zoneParcels(world, Three)

  def farmedParcelsForKitchen(parcels: Seq[Parcel], kitchen: Kitchen): Seq[Parcel] = parcels.filter(_.farmerID == kitchen.id)

  def farmedParcelsForKitchen(world: World, kitchen: Kitchen): Seq[Parcel] = farmedParcelsForKitchen(world.parcels, kitchen)

  def ownedParcelsForKitchen(world: World, kitchen: Kitchen) = world.parcels.filter(_.ownerID == kitchen.id)

  def parcelsInCultureForKitchen(world: World, kitchen: Kitchen): Seq[Parcel] = parcelsInCultureForKitchen(world.parcels, kitchen)

  def parcelsInCultureForKitchen(parcels: Seq[Parcel], kitchen: Kitchen): Seq[Parcel] = parcels.filter { p => p.farmerID == kitchen.id && Parcel.isCultivated(p) }

  def notAssignedParcels(world: World) = world.parcels.filter(_.crop == NotAssigned)

  def milParcels(world: World) = world.parcels.filter(_.crop == Mil)

  def peanutParcels(world: World) = world.parcels.filter(_.crop == Peanut)

  def fallowParcels(world: World) = world.parcels.filter(_.crop == Fallow)
}

case class World(parcels: Seq[Parcel], highestKitckenID: KitchenID)
