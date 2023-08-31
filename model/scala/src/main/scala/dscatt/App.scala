package dscatt

import dscatt.Croping._
import dscatt.KitchenPartition._

object Diohine {

  case class Parameters(outputParcelPath: Option[String] = None)

  def main(args: Array[String]) = {
    val parameters = if (args.length > 0) {
      Parameters(Some(args(0)))
    } else Parameters()

    val manureDepositStategyALL = (p: Parcel) => true
    val manureDepositStategyMilNextYear = { (p: Parcel, r: RotationCycle) =>
      Croping.evolveCrop(p.crop, r, Croping.evolveCropZone(p.cropZone, r)) == Mil
    }

    val kitchenProfile1 = KitchenProfile(
      5,
      ThreeYears,
      Parsimonious,
      NeverUseFallow,
      ExtraParcelsExceptFallowLoaner,
      FoodForAllStrategy,
      10,
      EverywhereByDayOwnerByNight,
      EverywhereByDayOwnerByNight,
      manureDepositStategyMilNextYear,
      UniformFertilizing,
      Mulching(0.5)
    )

    val kitchenProfile2 = KitchenProfile(
      5,
      ThreeYears,
      Parsimonious,
      NeverUseFallow,
      ExtraParcelsExceptFallowLoaner,
      FoodForAllStrategy,
      5,
      EverywhereByDayOwnerByNight,
      EverywhereByDayOwnerByNight,
      manureDepositStategyMilNextYear,
      UniformFertilizing,
      Mulching(0.5)
    )
    // val kitchenProfile2 = KitchenProfile(10, TwoYears, Parsimonious, AllExtraParcelsLoaner, FoodForAllStrategy)
    //    val kitchenProfile3 = KitchenProfile(10, ThreeYears, Parsimonious, AllExtraParcelsLoaner, FoodForUsOnlyStrategy)
    val kitchenPartition = KitchenPartition((kitchenProfile1, 10), (kitchenProfile2, 10))
    /*, (kitchenProfile2, 16)),(kitchenProfile3, 8)),*/
    val supportPolicy = SupportPolicy(taxPayerRatio = 1, fertilizerWeightPerYear = _ => kitchenPartition.profiles.map(_._2).sum * 20)

    Simulation(
      77,
      giniParcels = 0.05,
      giniTolerance = 0.05,
      maximumNumberOfParcels = 400,
      populationGrowth = 0.03,
      kitchenPartition = kitchenPartition,
      supportPolicy = supportPolicy,
      simulationLength = 10,
      parcelOutputPath = parameters.outputParcelPath)
  }

}

