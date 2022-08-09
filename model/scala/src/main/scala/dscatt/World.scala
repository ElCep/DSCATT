package dscatt

import fr.ign.artiscales.pm.parcel.SyntheticParcel
import fr.ign.artiscales.pm.{parcel, usecase}

import scala.jdk.CollectionConverters._
import Parcel._
import dscatt.Croping._
import dscatt.Kitchen.KitchenID

import java.io.File
import scala.annotation.tailrec

object World {

  def buildWorldGeometry(numberOfKitchen: Int,
                         giniIndex: Double,
                         giniTolerance: Double = 0.01,
                         maximumNumberOfParcels: Int = 200,
                         geometryImagePath: Option[String] = None
                        ): World = {

    geometryImagePath.foreach { p =>
      new File(p).mkdirs
    }
    val syntheticParcels = usecase.GenerateSyntheticParcel.generate(
      numberOfKitchen, giniIndex, maximumNumberOfParcels, giniTolerance.toFloat, new java.io.File(geometryImagePath.getOrElse(null)))

    val parcels = syntheticParcels.toArray.map { sp =>
      val p = sp.asInstanceOf[SyntheticParcel]

      val cropZone: CropZone = {
        if (p.distanceToCenter < Constants.VILLAGE_ZONE_DISTANCE) Village
        else p.regionID
      }

      val crop = cropZone match {
        case Village => HutField
        case _ => NotAssigned
      }

      Parcel(
        id = p.id,
        kitchenID = p.ownerID,
        crop = crop,
        cropZone = cropZone,
        area = p.area * Constants.AREA_FACTOR,
        distanceToVillage = p.distanceToCenter,
        neighbours = p.lIdNeighborhood.asScala.toSeq
      )
    }

    World(parcels)
  }

  def evolveRotations(world: World, kitchens: Seq[Kitchen]): World = {

    val inCulture = kitchens.flatMap { k =>
      val parcelsForKitchenK = World.parcelsForKitchen(world, k).filterNot(p => p.crop == HutField)

      k.cropingStrategy match {
        case Parsimonious =>
          val sortedByDistanceParcels = parcelsForKitchenK.sortBy(_.distanceToVillage)
          val needs = Kitchen.foodNeeds(k)

          @tailrec
          def cropsToBeCultivated(kitchen: Kitchen, production: Double, sortedParcels: Seq[Parcel], inCulture: Seq[Parcel]): Seq[Parcel] = {
            if (sortedParcels.isEmpty || production > needs) {
              inCulture
            }
            else {
              val parcel = sortedParcels.head
              cropsToBeCultivated(kitchen, production + Kitchen.parcelProduction(parcel), sortedParcels.tail, inCulture :+ parcel)
            }
          }

          cropsToBeCultivated(k, 0.0, sortedByDistanceParcels, Seq())
        case Intensive => parcelsForKitchenK
      }
    }
    
    world.copy(parcels = world.parcels.map { p =>
      Kitchen.kitchen(kitchens, p.kitchenID).map { k =>
        val newCropZone = Croping.evolveCropZone(p.cropZone, k.rotationCycle)
        p.copy(cropZone = newCropZone,
          crop = Croping.evolveCrop(p.crop, k.rotationCycle, newCropZone, inCulture.contains(p))
        )
      }.getOrElse(p)
    })
  }

  def display(world: World): Unit = {
    world.parcels.foreach { p =>
      println("ID              :" + p.id)
      println("KITCHEN         :" + p.kitchenID)
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

  def zoneVillageParcels(world: World) = zoneParcels(world, Village)

  def parcelsForKitchen(world: World, kitchen: Kitchen) = world.parcels.filter(_.kitchenID == kitchen.id)

  def parcelsInCultureForKitchen(world: World, kitchen: Kitchen) = world.parcels.filter { p => p.kitchenID == kitchen.id && Parcel.isCultivated(p) }

  def notAssignedParcels(world: World) = world.parcels.filter(_.crop == NotAssigned)

  def milParcels(world: World) = world.parcels.filter(_.crop == Mil)

  def peanutParcels(world: World) = world.parcels.filter(_.crop == Peanut)

  def fallowParcels(world: World) = world.parcels.filter(_.crop == Fallow)
}

case class World(parcels: Seq[Parcel])
