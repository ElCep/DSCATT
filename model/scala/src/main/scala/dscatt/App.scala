package dscatt

object Diohine{

  case class Parameters(outputParcelPath: Option[String] = None)


  def main(args: Array[String]) = {
    val parameters = if (args.length > 0) {
      Parameters(Some(args(0)))
    } else Parameters()

    Simulation(
      77,
      numberOfKitchens = 12,
      kitchenSizeAverage = 5.0,
      kitchenSizeStd = 1.0,
      giniParcels = 0.5,
      giniTolerance = 0.01,
      maximumNumberOfParcels = 200,
      parcelOutputPath = parameters.outputParcelPath)
  }

}

