package dscatt

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
             parcelOutputPath: Option[String] = None
           ) = {

    given MersenneTwister(seed)

    val kitchens = Kitchen.buildKitchens(numberOfKitchens, kitchenSizeAverage, kitchenSizeStd)

    val diohine = World.buildWorldGeometry(numberOfKitchens, giniParcels, giniTolerance, maximumNumberOfParcels, parcelOutputPath)
    World.display(diohine)
  }
}
