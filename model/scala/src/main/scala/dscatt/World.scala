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

    case class CultureAndManPower(evolvedCropsParcels: Seq[Parcel], manPowerBalance: Double)

    val newParcelsWithCrops = kitchens.flatMap { k =>

      // Compute theoritical crops for coming year before we know if it is in culture or note
      val parcelCandidatesForKitchenK = World.parcelsForKitchen(world, k).map { p =>
        val newCropZone = Croping.evolveCropZone(p.cropZone, k.rotationCycle)
        p.copy(cropZone = newCropZone,
          crop = Croping.evolveCrop(p.crop, k.rotationCycle, newCropZone)
        )
      }

      val (notCultivableCandidatesForKitchenK, cultivableCandidatesForKitchenK) = parcelCandidatesForKitchenK.partition { p =>
        // Exclude hutfields and next year fallow parcels
        p.crop == HutField || p.crop == Fallow
      }

      k.cropingStrategy match {
        case Parsimonious =>
          val parcels = Kitchen.cropNeeds(k, cultivableCandidatesForKitchenK, Kitchen.foodNeeds(k))
          parcels ++: notCultivableCandidatesForKitchenK
        case Provisioning(exceedingProportion) =>
          val parcels = Kitchen.cropNeeds(k, cultivableCandidatesForKitchenK, Kitchen.foodNeeds(k) * (1 + exceedingProportion))
          parcels ++: notCultivableCandidatesForKitchenK
        case AsMuchAsWeCan =>
          // Needs are set on purpose to a big value so that it does not limit cultivation
          Kitchen.cropNeeds(k, cultivableCandidatesForKitchenK, Kitchen.foodNeeds(k) * 10000) ++: notCultivableCandidatesForKitchenK
      }
    }

    world.copy(parcels = newParcelsWithCrops)

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
