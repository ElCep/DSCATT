package dscatt

import dscatt.Croping._
import org.apache.commons.math3.random.MersenneTwister

import scala.annotation.tailrec

object Simulation {

  def apply(
             seed: Long,
             numberOfKitchens: Int,
             kitchenSizeAverage: Double,
             kitchenSizeStd: Double,
             giniParcels: Double,
             giniTolerance: Double = 0.01,
             maximumNumberOfParcels: Int = 200,
             herdSize: Int = 100,
             giniHerd: Double,
             populationGrowth: Double,
             rotationCycle: RotationCycle = ThreeYears,
             cropingStrategy: CropingStrategy = Parsimonious,
             simulationLength: Int = 20,
             parcelOutputPath: Option[String] = None
           ) = {

    given MersenneTwister(seed)

    val kitchens = Kitchen.buildKitchens(numberOfKitchens, kitchenSizeAverage, kitchenSizeStd)
    val nakedWorld = World.buildWorldGeometry(numberOfKitchens, giniParcels, giniTolerance, maximumNumberOfParcels, parcelOutputPath)

    // Initialize first rotation
    val firstworld = World.evolveRotations(nakedWorld, kitchens, rotationCycle, cropingStrategy)

    println("--------------- FIN INIT ---------------- " + firstworld.parcels.length)
    println("FALLOW " + World.fallowParcels(firstworld).length)
    println("PEANUT " + World.peanutParcels(firstworld).length)
    println("MIL " + World.milParcels(firstworld).length)
    println("NOT ASSIGNED " + firstworld.parcels.filter { p => p.crop == NotAssigned }.length)
    println("HUT Fields " + firstworld.parcels.filter { p => p.crop == HutField }.length)


    println("Area factor " + Constants.AREA_FACTOR)

    evolve(firstworld, kitchens, rotationCycle, cropingStrategy, populationGrowth, simulationLength)
  }

  case class Indicators(
                         fertility: Double = 0.0,
                         populationSize: Int = 0,
                         nbEmigrantsPerYear: Seq[Int] = Seq()
                       )

  def evolve(world: World, kitchens: Seq[Kitchen], rotationCycle: RotationCycle, cropingStrategy: CropingStrategy, populationGrowth: Double, simulationLenght: Int)(using MersenneTwister) = {

    @tailrec
    def evolve0(world: World, kitchens: Seq[Kitchen], year: Int, indicators: Indicators): Indicators = {
      if (year == 0) indicators
      else {
        println("\nYEAR " + (simulationLenght - year))


        val (upToDateKitchens, upToDateWorld) = Kitchen.evolve(world, kitchens, populationGrowth)
        println("REMAINING KITCHENS " + upToDateKitchens.map(_.id).mkString(" | ") + "--- NB KITCHENS " + upToDateKitchens.length)
        println("KITCHEN IDS IN WORLD " + upToDateWorld.parcels.map{_.kitchenID}.distinct.sorted)
        println("NB PARCELS PER KID " + upToDateWorld.parcels.map{_.kitchenID}.groupBy(x=>x).map(e => (e._1, e._2.length)))

        val newWorld = World.evolveRotations(upToDateWorld, upToDateKitchens, rotationCycle, cropingStrategy)

        println(" ---- EVOLVED ---- " + newWorld.parcels.length)
        println("FALLOW " + World.fallowParcels(newWorld).length)
        println("PEANUT " + World.peanutParcels(newWorld).length)
        println("MIL " + World.milParcels(newWorld).length)
        println("SUM " + (World.fallowParcels(newWorld).length + World.milParcels(newWorld).length + World.peanutParcels(newWorld).length))
        println("NOT ASSIGNED " + newWorld.parcels.filter { p => p.crop == NotAssigned }.length)
        println("HUT Fields " + newWorld.parcels.filter { p => p.crop == HutField }.length)

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
