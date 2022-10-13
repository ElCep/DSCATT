package dscatt

import dscatt.Croping._
import dscatt.KitchenPartition._

object Diohine {

  case class Parameters(outputParcelPath: Option[String] = None)
  
  def main(args: Array[String]) = {
    val parameters = if (args.length > 0) {
      Parameters(Some(args(0)))
    } else Parameters()


    val kitchenProfile1 = KitchenProfile(10, ThreeYears, Parsimonious, AllExtraParcelsLoaner)
    val kitchenProfile2 = KitchenProfile(10, ThreeYears, AsMuchAsWeCan, Selfish)


    Simulation(
      77,
      giniParcels = 0.0,
      giniTolerance = 0.01,
      maximumNumberOfParcels = 150,
      herdSize = 100,
      giniHerd = 75,
      populationGrowth = 0.03,
      kitchenPartition = KitchenPartition((kitchenProfile1, 5) , (kitchenProfile2, 10)),
      simulationLength = 10,
      parcelOutputPath = parameters.outputParcelPath)
  }

}

