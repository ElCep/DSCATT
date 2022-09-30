package dscatt

import dscatt.Croping._
import org.apache.commons.math3.random.MersenneTwister

import scala.annotation.tailrec

object Simulation {

  def apply(
             seed: Long,
             giniParcels: Double,
             giniTolerance: Double = 0.01,
             maximumNumberOfParcels: Int = 200,
             herdSize: Int = 100,
             giniHerd: Double,
             populationGrowth: Double,
             kitchenPartition: KitchenPartition = KitchenPartition((KitchenProfile.default, 1)),
             simulationLength: Int = 20,
             parcelOutputPath: Option[String] = None
           ) = {

    given MersenneTwister(seed)

    val kitchens = Kitchen.buildKitchens(kitchenPartition)

//    val newK = Population.evolve(kitchens, populationGrowth)
//    newK.foreach {k=>
//      println(k.id + " :: " + k.size)
//    }
    println("NB KITCH " + kitchens.length)
    val nakedWorld = World.buildWorldGeometry(kitchens.length, giniParcels, giniTolerance, maximumNumberOfParcels, seed, parcelOutputPath)

    val oo = nakedWorld.parcels.groupBy(_.kitchenID).map { x => x._2.map {
      _.area
    }.sum.ceil
    }
    println(oo)
    println("MEAN " + oo.sum / nakedWorld.parcels.length)
    // Initialize first rotation
    val firstworld = Rotation.evolve(nakedWorld, kitchens)

       println("--------------- FIN INIT ---------------- " + firstworld.parcels.length)
    println("FALLOW " + World.fallowParcels(firstworld).length)
    println("PEANUT " + World.peanutParcels(firstworld).length)
    println("MIL " + World.milParcels(firstworld).length)
    println("NOT ASSIGNED " + firstworld.parcels.filter { p => p.crop == NotAssigned }.length)


    println("Area factor " + Constants.AREA_FACTOR)

    evolve(firstworld, kitchens, populationGrowth, simulationLength)
  }

  case class Indicators(
                         fertility: Double = 0.0,
                         populationSize: Int = 0,
                         nbEmigrantsPerYear: Seq[Int] = Seq()
                       )

  def evolve(world: World, kitchens: Seq[Kitchen], populationGrowth: Double, simulationLenght: Int)(using MersenneTwister) = {

    @tailrec
    def evolve0(world: World, kitchens: Seq[Kitchen], year: Int, indicators: Indicators): Indicators = {
      if (year == 0) indicators
      else {
        println("\n((((((((((((((((((((((YEAR " + (simulationLenght - year) + ")))))))))))))))))))))))))))))")

        val (upToDateKitchens, upToDateWorld) = Kitchen.evolve(world, kitchens, populationGrowth)
        println("REMAINING KITCHENS " + upToDateKitchens.map(_.id).sorted.mkString(" | ") + "--- NB KITCHENS " + upToDateKitchens.length)
        println("KITCHEN SIZES " + upToDateKitchens.map{k=> k.id-> k.size})

        val newWorld = Rotation.evolve(upToDateWorld, upToDateKitchens)


//        println("************ ")
//        val ooo = World.notAssignedParcels(newWorld)
//        kitchens.foreach{k=>
//          println((k.id, k.size, Kitchen.foodBalance(newWorld,k), Kitchen.cultivatedSurface(newWorld, k), World.parcelsForKitchen(newWorld,k).length, ooo.filter{_.kitchenID == k.id}.map{_.area}.sum))
//        }

        println(" ---- EVOLVED ---- " + newWorld.parcels.length)
        println("FALLOW " + World.fallowParcels(newWorld).length)
        println("PEANUT " + World.peanutParcels(newWorld).length)
        println("MIL " + World.milParcels(newWorld).length)
        println("SUM " + (World.fallowParcels(newWorld).length + World.milParcels(newWorld).length + World.peanutParcels(newWorld).length))
        println("NOT ASSIGNED " + newWorld.parcels.filter { p => p.crop == NotAssigned }.length)

        println("\nZONE 1 : " + World.zoneOneParcels(newWorld).length)
        println("ZONE 2 : " + World.zoneTwoParcels(newWorld).length)
        println("ZONE 3: " + World.zoneThreeParcels(newWorld).length)
        //---------------------------------------------------------------------------------------

        evolve0(newWorld, upToDateKitchens, year - 1, indicators)
      }
    }

    // The first year (+1) is an initialization for kitchen needs
    evolve0(world, kitchens, simulationLenght, Indicators())
  }
}
