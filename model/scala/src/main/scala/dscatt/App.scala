package dscatt

import dscatt.Croping._
import dscatt.KitchenPartition._

object Diohine {

  case class Parameters(outputParcelPath: Option[String] = None)
  
  def main(args: Array[String]) = {
    val parameters = if (args.length > 0) {
      Parameters(Some(args(0)))
    } else Parameters()


    val kitchenProfile1 = KitchenProfile(4, ThreeYears, Parsimonious, AllExtraParcelsLoaner)
    val kitchenProfile2 = KitchenProfile(10, TwoYears, AsMuchAsWeCan, Selfish)


    Simulation(
      77,
      giniParcels = 0.4,
      giniTolerance = 0.01,
      maximumNumberOfParcels = 200,
      herdSize = 100,
      giniHerd = 75,
      populationGrowth = 0.05,
      kitchenPartition = KitchenPartition((kitchenProfile1, 10) , (kitchenProfile2, 10)),
      simulationLength = 12,
      parcelOutputPath = parameters.outputParcelPath)
  }

}

