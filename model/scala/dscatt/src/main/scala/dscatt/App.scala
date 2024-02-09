package dscatt

import Croping._
import KitchenPartition._
import utils.*
import Data.*

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
      displayKitchens = true,
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
      LoanStrategy.AllExtraParcelsLoaner,
      FoodDonationStrategy.FoodForAllStrategy,
      HerdStrategy.EverywhereByDayOwnerByNight,
      HerdStrategy.EverywhereByDayOwnerByNight,
      manureDepositStategyMilNextYear,
      FertilizerStrategy.UniformFertilizing,
      MulchingStrategy.Mulching(0.0),
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
    val kitchenPartition = KitchenPartition((kitchenProfile1, 30))
    /*, (kitchenProfile2, 16)),(kitchenProfile3, 8)),*/
    val supportPolicy = SupportPolicy(taxPayerRatio = 1, fertilizerWeightPerYear = _ => kitchenPartition.profiles.map(_._2).sum * 20)

    val (simulationState, simulationData) = Simulation(
      7L,
      giniParcels = 0.2,
      populationGrowth = 0.02,
      kitchenPartition = kitchenPartition,
      supportPolicy = supportPolicy,
      simulationLength = 26,
      soilQualityBasis = 0.5312696251467559,
      fallowBoost = 0.2436522594214085,
      peanutSeedToFood = 1.2230584989744784,
      expandingHerdSize = 1.314401864796489,
      dailyFoodNeedPerPerson = 0.555,
      //      soilQualityBasis = 0.037566386501967745,
      //      fallowBoost = 1.1600945798567732,
      //      peanutSeedToFood = 1.379729887022548,
      //      expandingHerdSize = 1.6991464444068525,
      hookParameters = hooks,
      rainFall = 600
    )

    given data: Data = simulationData

    val (rsquare, slope) = simulationState.populationRSquareAndSlope
    println("Pop " + simulationState.populationDynamic.toSeq)
    println("\nPop R2 " + rsquare)
    println("\nPop slope " + slope)
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
    println("\nEffective fallow " + simulationState.effectiveFallowRatioDynamic.toSeq)
    println("\nFood stress " + simulationState.foodStress.toSeq)
    println("\nMil yield dynamic  " + simulationState.averageMilYieldDynamic.toSeq)
    println("\nPeanut yield dynamic  " + simulationState.averagePeanutYieldDynamic.toSeq)
    println("\nNb of kitchens " + simulationState.numberOfKitchens.toSeq)
  }


}

