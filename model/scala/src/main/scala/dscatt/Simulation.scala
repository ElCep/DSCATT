package dscatt

import dscatt.Croping.{CroppingStrategy, Fallow, Mil, NotAssigned, Peanut, ThreeYears, Village}
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
             demographicGrowth: Double,
             croppingStrategy: CroppingStrategy = ThreeYears,
             simulationLength: Int = 20,
             parcelOutputPath: Option[String] = None
           ) = {

    given MersenneTwister(seed)

    val kitchens = Kitchen.buildKitchens(numberOfKitchens, kitchenSizeAverage, kitchenSizeStd)
    val diohine = World.buildWorldGeometry(numberOfKitchens, giniParcels, giniTolerance, maximumNumberOfParcels, parcelOutputPath)

    // Initialize first rotation

    // World.display(diohine)
    println("# Vilage " + World.zoneVillageParcels(diohine).length)
    println("Area factor " + Constants.AREA_FACTOR)

    evolve(diohine, kitchens, ThreeYears, simulationLength)
  }

  case class Indicators(fertility: Double = 0.0, populationSize: Int = 0)

  def evolve(world: World, kitchens: Seq[Kitchen], croppingStrategy: CroppingStrategy, simulationLenght: Int) = {

    @tailrec
    def evolve0(world: World, kitchens: Seq[Kitchen], year: Int, indicators: Indicators): Indicators = {
      if (year == 0) indicators
      else {
        println("\nYEAR " + (simulationLenght - year + 1))

        val newWorld = World.evolveRotations(world, kitchens, croppingStrategy)

        println(" ---- EVOLVED ---- ")
        println("FALLOW " + World.fallowParcels(newWorld).length)
        println("PEANUT " + World.peanutParcels(newWorld).length)
        println("MIL " + World.milParcels(newWorld).length)
        println("NOT ASSIGNED " + newWorld.parcels.filter { p => p.crop == NotAssigned && p.cropZone != Village }.length)

        //---------------------------------------------------------------------------------------

        evolve0(newWorld, kitchens, year - 1, indicators)
      }
    }

    // The first year (+1) is an initialization for kitchen needs
    evolve0(world, kitchens, simulationLenght + 1, Indicators())
  }
}
