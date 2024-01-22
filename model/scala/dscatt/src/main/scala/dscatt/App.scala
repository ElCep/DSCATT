package dscatt

import Croping._
import KitchenPartition._
import utils.*

object Diohine {

  case class HookFile(outputPath: String, parcels: Boolean, kitchens: Boolean)

  case class HookParameters(displayParcels: Boolean = true, displayKitchens: Boolean = false, hookFile: Option[HookFile])

  def main(args: Array[String]) = {

    val hookFile = HookFile(
      outputPath = "/tmp",
      parcels = false,
      kitchens = false
    )

    val hooks = HookParameters(
      displayParcels = false,
      displayKitchens = false,
      hookFile = Some(hookFile)
    )

    val manureDepositStategyMilNextYear = { (p: Parcel, r: RotationCycle) =>
      Croping.evolveCrop(p.crop, r, Croping.evolveCropZone(p.cropZone, r)) == Mil
    }

    val kitchenProfile1 = KitchenProfile(
      kitchenSize = 16,
      RotationCycle.ThreeYears,
      CropingStrategy.PeanutForInexcess(0.0),
      OwnFallowUse.NeverUseFallow,
      LoanStrategy.ExtraParcelsExceptFallowLoaner,
      FoodDonationStrategy.FoodForAllStrategy,
      HerdStrategy.EverywhereByDayOwnerByNight,
      HerdStrategy.EverywhereByDayOwnerByNight,
      manureDepositStategyMilNextYear,
      FertilizerStrategy.UniformFertilizing,
      MulchingStrategy.Mulching(0.50),
      4
    )

//    val kitchenProfile2 = KitchenProfile(
//      kitchenSize = 16,
//      RotationCycle.ThreeYears,
//      CropingStrategy.PeanutForInexcess(0.5),
//      OwnFallowUse.NeverUseFallow,
//      LoanStrategy.ExtraParcelsExceptFallowLoaner,
//      FoodDonationStrategy.FoodForAllStrategy,
//      HerdStrategy.EverywhereByDayOwnerByNight,
//      HerdStrategy.EverywhereByDayOwnerByNight,
//      manureDepositStategyMilNextYear,
//      FertilizerStrategy.UniformFertilizing,
//      MulchingStrategy.Mulching(0.10),
//      4
//    )
    // val kitchenProfile2 = KitchenProfile(10, TwoYears, Parsimonious, AllExtraParcelsLoaner, FoodForAllStrategy)
    //    val kitchenProfile3 = KitchenProfile(10, ThreeYears, Parsimonious, AllExtraParcelsLoaner, FoodForUsOnlyStrategy)
    val kitchenPartition = KitchenPartition((kitchenProfile1, 23))
    /*, (kitchenProfile2, 16)),(kitchenProfile3, 8)),*/
    val supportPolicy = SupportPolicy(taxPayerRatio = 1, fertilizerWeightPerYear = _ => kitchenPartition.profiles.map(_._2).sum * 20)

    val simulationState = Simulation(
      77,
      giniParcels = 0.1,
      populationGrowth = 0.021,
      kitchenPartition = kitchenPartition,
      supportPolicy = supportPolicy,
      simulationLength = 26,
      soilQualityBasis = 0.0,
      erosion = 0.001,
      fallowBoost = 2.0,
      kitchenMinimumSize = 4,
      kitchenMaximumSize = 22,
      splitKitchenOffspringSize = 8,
      peanutSeedToFood = 0.88,
      hookParameters = hooks)

    println("Pop " + simulationState.populationDynamic.toSeq)
    println("\nPop R2 " + simulationState.populationRSquare)
    println("\nMigrant dynamic  " + simulationState.migrantsDynamic.toSeq)
    println("\nherd " + simulationState.herdDynamic.toSeq)
    println("\nnitrogen " + simulationState.averageNitrogenDynamic.toSeq)
    println("\nSoil Quality " + simulationState.averageSoilQualityDynamic.toSeq)
    println("\nAverage Inexesse " + simulationState.averageInexcessDynamic.toSeq)
    println("\n# unbalanced kitchen " + simulationState.numberOfUnbalancedKitchen)
    println("\nTotal Loaned Area " + simulationState.totalLoanedArea)
    println("\nManure dynamic " + simulationState.averageManureDynamic.toSeq)
    println("\nTotal Manure  " + simulationState.totalManure)
    println("\nMulching dynamic " + simulationState.averageMulchingDynamic.toSeq)
    println("\nTotal Mulching  " + simulationState.totalMulching)
    println("\nFFL on Food needs dynamic  " + simulationState.foodFromLoanOnFoodNeedsDynamic.toSeq)
    println("\nFFD on Food needs dynamic  " + simulationState.foodFromDonationOnFoodNeedsDynamic.toSeq)
    println("\nKitchen size  " + simulationState.averageKitchenSizeDynamic.toSeq)
    println("\nKSA " + average(simulationState.averageKitchenSizeDynamic.toSeq))
    println("\nMil yield dynamic  " + simulationState.averageMilYieldDynamic.toSeq)
    println("\nPeanut yield dynamic  " + simulationState.averagePeanutYieldDynamic.toSeq)
    println("\nFood stress " + simulationState.foodStress)
  }


}

