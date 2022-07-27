package dscatt

import dscatt.Croping._

object Diohine {

  case class Parameters(outputParcelPath: Option[String] = None)
  
  def main(args: Array[String]) = {
    val parameters = if (args.length > 0) {
      Parameters(Some(args(0)))
    } else Parameters()

    Simulation(
      77,
      numberOfKitchens = 20,
      kitchenSizeAverage = 20.0,
      kitchenSizeStd = 0.0,
      giniParcels = 0.0,
      giniTolerance = 0.01,
      maximumNumberOfParcels = 200,
      herdSize = 100,
      giniHerd = 75,
      populationGrowth = 0.2,
      rotationCycle = ThreeYears,
      cropingStrategy = Parsimonious,
      simulationLength = 8,
      parcelOutputPath = parameters.outputParcelPath)
  }

}

