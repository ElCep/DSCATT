package dscatt

import dscatt.Croping._
import dscatt.KitchenPartition._

object Diohine {

  case class Parameters(outputParcelPath: Option[String] = None)
  
  def main(args: Array[String]) = {
    val parameters = if (args.length > 0) {
      Parameters(Some(args(0)))
    } else Parameters()


    val kitchenProfile1 = KitchenProfile(18, ThreeYears, Parsimonious, Selfish)
    val kitchenProfile2 = KitchenProfile(4, ThreeYears, AsMuchAsWeCan, Selfish)


    Simulation(
      77,
      giniParcels = 0.0,
      giniTolerance = 0.01,
      maximumNumberOfParcels = 350,
      herdSize = 100,
      giniHerd = 75,
      populationGrowth = 0.03,
      kitchenPartition = KitchenPartition((kitchenProfile1, 10) , (kitchenProfile2, 15)),
      simulationLength = 15,
      parcelOutputPath = parameters.outputParcelPath)
  }

}

