package dscatt

import fr.ign.artiscales.pm.parcel.SyntheticParcel
import fr.ign.artiscales.pm.{parcel, usecase}

import scala.jdk.CollectionConverters.*
import Parcel.*
import dscatt.Croping.*
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

    geometryImagePath.foreach{p=>
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

      Parcel(
        id = p.id,
        kitchenID = p.ownerID,
        crop = NotAssigned,
        cropZone = cropZone,
        area = p.area * Constants.AREA_FACTOR,
        distanceToVillage = p.distanceToCenter,
        neighbours = p.lIdNeighborhood.asScala.toSeq
      )
    }

    World(parcels)
  }

  def evolveRotations(world: World, kitchens: Seq[Kitchen], cropingStrategy: CroppingStrategy): World = {

    // Initalization case
    if (World.notAssignedParcels(world).length == world.parcels.length) {
      world.copy(parcels = world.parcels.map { p =>
        p.copy(crop = Croping.evolveCrop(p.crop, cropingStrategy, p.cropZone))
      })
    }
    else {
      val inCulture = kitchens.flatMap { k =>
        val sortedByDistanceParcels = World.parcelsForKitchen(world, k).sortBy(_.distanceToVillage) //.filterNot(_.crop == Fallow)
        val needs = Kitchen.foodNeeds(k)

        @tailrec
        def inCultureCrops(kitchen: Kitchen, production: Double, sortedParcels: Seq[Parcel], inCulture: Seq[Parcel]): Seq[Parcel] = {
          if (sortedParcels.isEmpty || production > needs) {
            println(" --- OUT ::  " + sortedParcels.length + " // " + production + " // " + needs)
            inCulture
          }
          else {
            val parcel = sortedParcels.head
            inCultureCrops(kitchen, production + Kitchen.parcelProduction(parcel), sortedParcels.tail, inCulture :+ parcel)
          }
        }

        inCultureCrops(k, 0.0, sortedByDistanceParcels, Seq())
      }

      world.copy(parcels = world.parcels.map { p =>
        val newCropZone = Croping.evolveCropZone(p.cropZone, cropingStrategy)
        p.copy(cropZone = newCropZone,
          crop = {
            if (inCulture.contains(p)) Croping.evolveCrop(p.crop, cropingStrategy, newCropZone)
            else {
              if (p.crop == Peanut) Fallow
              else NotAssigned
            }
          })
      })
    }
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
