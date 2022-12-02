package dscatt

import dscatt.Croping._
import dscatt.KitchenPartition._

object Diohine {

  case class Parameters(outputParcelPath: Option[String] = None)

  def main(args: Array[String]) = {
    val parameters = if (args.length > 0) {
      Parameters(Some(args(0)))
    } else Parameters()


    val kitchenProfile1 = KitchenProfile(10, ThreeYears, Parsimonious, ExtraParcelsExceptFallowLoaner, FoodForAllStrategy, 15, EverywhereByDayOwnerByNight, EverywhereByDayOwnerByNight, UniformFertilizing)
    // val kitchenProfile2 = KitchenProfile(10, TwoYears, Parsimonious, AllExtraParcelsLoaner, FoodForAllStrategy)
    //    val kitchenProfile3 = KitchenProfile(10, ThreeYears, Parsimonious, AllExtraParcelsLoaner, FoodForUsOnlyStrategy)
    val kitchenPartition = KitchenPartition((kitchenProfile1, 10)) /*, (kitchenProfile2, 16)),(kitchenProfile3, 8)),*/
    val supportPolicy = SupportPolicy(taxPayerRatio = 1, fertilizerWeightPerYear = _ => kitchenPartition.profiles.map(_._2).sum * 20)

    Simulation(
      77,
      giniParcels = 0.0,
      giniTolerance = 0.05,
      maximumNumberOfParcels = 200,
      populationGrowth = 0.03,
      kitchenPartition = kitchenPartition,
      supportPolicy = supportPolicy,
      simulationLength = 10,
      parcelOutputPath = parameters.outputParcelPath)
  }

}

