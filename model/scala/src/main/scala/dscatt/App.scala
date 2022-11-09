package dscatt

import dscatt.Croping._
import dscatt.KitchenPartition._

object Diohine {

  case class Parameters(outputParcelPath: Option[String] = None)
  
  def main(args: Array[String]) = {
    val parameters = if (args.length > 0) {
      Parameters(Some(args(0)))
    } else Parameters()


    val kitchenProfile1 = KitchenProfile(14, ThreeYears, Parsimonious, ExtraParcelsExceptFallowLoaner)
    val kitchenProfile2 = KitchenProfile(4, ThreeYears, AsMuchAsWeCan, ExtraParcelsExceptFallowLoaner)


    Simulation(
      77,
      giniParcels = 0.0,
      giniTolerance = 0.05,
      maximumNumberOfParcels = 200,
      herdSize = 100,
      giniHerd = 75,
      populationGrowth = 0.03,
      kitchenPartition = KitchenPartition((kitchenProfile1, 12) , (kitchenProfile2, 2)),
      simulationLength = 12,
      parcelOutputPath = parameters.outputParcelPath)
  }

}

