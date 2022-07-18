package dscatt

import dscatt.Croping.{CropingStrategy, ThreeYears}
import dscatt.World.evolveRotations
import org.apache.commons.math3.random.MersenneTwister

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
             croppingStrategy: CropingStrategy = ThreeYears,
             simulationLength: Int = 20,
             parcelOutputPath: Option[String] = None
           ) = {

    given MersenneTwister(seed)

    val kitchens = Kitchen.buildKitchens(numberOfKitchens, kitchenSizeAverage, kitchenSizeStd)
    val nakedWorld = World.buildWorldGeometry(numberOfKitchens, giniParcels, giniTolerance, maximumNumberOfParcels, parcelOutputPath)

    // Initialize first rotation
    val diohine = World.evolveRotations(nakedWorld, croppingStrategy)

   // World.display(diohine)

    evolve(diohine, kitchens, ThreeYears, 4)
  }

  case class Indicators(fertility: Double = 0.0, populationSize: Int = 0)

  def evolve(world: World, kitchens: Seq[Kitchen], cropingStrategy: CropingStrategy, simulationLenght: Int) = {

    def evolve0(world: World, kitchens: Seq[Kitchen], year: Int, indicators: Indicators): Indicators = {
      if (year == 0) indicators
      else {
        val newWorld = evolveRotations(world, cropingStrategy)

        println("YEAR " + (simulationLenght - year + 1))
        world.parcels.take(5).foreach { p =>
          println(p.id + " " + p.rotation.cropZone.getClass.toString + " " + p.rotation.crop.getClass.toString)
        }

        evolve0(newWorld, kitchens, year - 1, indicators)
      }
    }

    evolve0(world, kitchens, simulationLenght, Indicators())
  }
}
