package dscatt

import dscatt.Croping._
import dscatt.KitchenPartition._

object Diohine {

  case class Parameters(outputParcelPath: Option[String] = None)
  
  def main(args: Array[String]) = {
    val parameters = if (args.length > 0) {
      Parameters(Some(args(0)))
    } else Parameters()


    val kitchenProfile1 = KitchenProfile(10, ThreeYears, Parsimonious, ExtraParcelsExceptFallowLoaner, FoodForAllStrategy)
//    val kitchenProfile2 = KitchenProfile(10, ThreeYears, Parsimonious, AllExtraParcelsLoaner, FoodForUsOnlyStrategy)
//    val kitchenProfile3 = KitchenProfile(10, ThreeYears, Parsimonious, AllExtraParcelsLoaner, FoodForUsOnlyStrategy)


    Simulation(
      77,
      giniParcels = 0.0,
      giniTolerance = 0.05,
      maximumNumberOfParcels = 200,
      herdSize = 100,
      giniHerd = 75,
      populationGrowth = 0.03,
      kitchenPartition = KitchenPartition((kitchenProfile1, 20)/*, (kitchenProfile2, 8),(kitchenProfile3, 8)*/),
      simulationLength = 12,
      parcelOutputPath = parameters.outputParcelPath)
  }

}

