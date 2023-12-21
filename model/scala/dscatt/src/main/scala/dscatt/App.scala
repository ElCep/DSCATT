package dscatt

import Croping._
import KitchenPartition._

object Diohine {

  case class HookFile(outputPath: String, parcels: Boolean, kitchens: Boolean)

  case class HookParameters(displayParcels: Boolean = true, displayKitchens: Boolean = false, hookFile: Option[HookFile])

  def main(args: Array[String]) = {

    val hookFile = HookFile(
      outputPath = "/tmp",
      parcels = true,
      kitchens = true
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
      LoanStrategy.ExtraParcelsExceptFallowLoaner,
      FoodDonationStrategy.FoodForAllStrategy,
      HerdStrategy.EverywhereByDayOwnerByNight,
      HerdStrategy.EverywhereByDayOwnerByNight,
      manureDepositStategyMilNextYear,
      FertilizerStrategy.UniformFertilizing,
      MulchingStrategy.Mulching(0.1),
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
    val kitchenPartition = KitchenPartition((kitchenProfile1, 15))
    /*, (kitchenProfile2, 16)),(kitchenProfile3, 8)),*/
    val supportPolicy = SupportPolicy(taxPayerRatio = 1, fertilizerWeightPerYear = _ => kitchenPartition.profiles.map(_._2).sum * 20)

    val simulationState = Simulation(
      77,
      giniParcels = 0.2,
      populationGrowth = 0.021,
      kitchenPartition = kitchenPartition,
      supportPolicy = supportPolicy,
      simulationLength = 25,
      soilQualityBasis = 0.8,
      hookParameters = hooks)

    println("Pop " + simulationState.populationDynamic.toSeq)
    println("herd " + simulationState.herdDynamic.toSeq)
    println("nitrogen " + simulationState.averageNitrogenDynamic.toSeq)
    println("Soil Quality " + simulationState.averageSoilQualityDynamic.toSeq)
    println("Average Inexesse " + simulationState.averageInexcessDynamic)
    println("# unbalanced kitchen " + simulationState.numberOfUnbalancedKitchen)
    println("Total Loaned Area " + simulationState.totalLoanedArea)
    println("Manure dynamic " + simulationState.averageManureDynamic.toSeq)
    println("Total Manure  " + simulationState.totalManure)
    println("Mulching dynamic " + simulationState.averageMulchingDynamic.toSeq)
    println("Total Mulching  " + simulationState.totalMulching)
    println("Migrant dynamic  " + simulationState.migrantsDynamic.toSeq)
    println("FFL on Food needs dynamic  " + simulationState.foodFromLoanOnFoodNeedsDynamic.toSeq)
    println("FFD on Food needs dynamic  " + simulationState.foodFromDonationOnFoodNeedsDynamic.toSeq)
    println("Kitchen size  " + simulationState.averageKitchenSizeDynamic.toSeq)
  }


}

