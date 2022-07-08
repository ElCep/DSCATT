package dscatt

import fr.ign.artiscales.pm.parcel.SyntheticParcel
import fr.ign.artiscales.pm.{parcel, usecase}
import scala.jdk.CollectionConverters._
import Parcel._

object World {

  def buildWorldGeometry(numberOfKitchen: Int,
                         giniIndex: Double,
                         giniTolerance: Double = 0.01,
                         maximumNumberOfParcels: Int = 200,
                         geometryImagePath: Option[String] = None
                        ): World= {

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
            cropZone = cropZone,
            area = p.area,
            distanceToVillage = p.distanceToCenter,
            neighbours = p.lIdNeighborhood.asScala.toSeq
          )
      }

    World(parcels)
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
  
  private def zoneParcels(world: World, cropZone: CropZone) = world.parcels.filter{_.cropZone == cropZone}
  def zoneOneParcels(world: World) = zoneParcels(world, One)
  def zoneTwoParcels(world: World) = zoneParcels(world, Two)
  def zoneThreeParcels(world: World) = zoneParcels(world, Three)
  def zoneVillageParcels(world: World) = zoneParcels(world, Village)
}

case class World(parcels: Seq[Parcel])
