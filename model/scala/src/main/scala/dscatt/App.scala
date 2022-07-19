package dscatt

object Diohine {

  case class Parameters(outputParcelPath: Option[String] = None)
  
  def main(args: Array[String]) = {
    val parameters = if (args.length > 0) {
      Parameters(Some(args(0)))
    } else Parameters()

    Simulation(
      77,
      numberOfKitchens = 10,
      kitchenSizeAverage = 20.0,
      kitchenSizeStd = 2.0,
      giniParcels = 0.0,
      giniTolerance = 0.01,
      maximumNumberOfParcels = 200,
      herdSize = 100,
      giniHerd = 75,
      demographicGrowth = 0.2,
      simulationLength = 5,
      parcelOutputPath = parameters.outputParcelPath)
  }

}

