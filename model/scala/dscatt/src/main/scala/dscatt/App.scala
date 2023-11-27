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
      hookFile = None
    )

    val manureDepositStategyMilNextYear = { (p: Parcel, r: RotationCycle) =>
      Croping.evolveCrop(p.crop, r, Croping.evolveCropZone(p.cropZone, r)) == Mil
    }

    val kitchenProfile1 = KitchenProfile(
      kitchenSize = 10,
      RotationCycle.ThreeYears,
      CropingStrategy.PeanutForInexcess(0.0),
      OwnFallowUse.NeverUseFallow,
      LoanStrategy.ExtraParcelsExceptFallowLoaner,
      FoodDonationStrategy.FoodForAllStrategy,
      herdSize = 10,
      HerdStrategy.EverywhereByDayOwnerByNight,
      HerdStrategy.EverywhereByDayOwnerByNight,
      manureDepositStategyMilNextYear,
      FertilizerStrategy.UniformFertilizing,
      MulchingStrategy.Mulching(0.10)
    )

    val kitchenProfile2 = KitchenProfile(
      kitchenSize = 10,
      RotationCycle.ThreeYears,
      CropingStrategy.PeanutForInexcess(0.5),
      OwnFallowUse.NeverUseFallow,
      LoanStrategy.ExtraParcelsExceptFallowLoaner,
      FoodDonationStrategy.FoodForAllStrategy,
      herdSize = 5,
      HerdStrategy.EverywhereByDayOwnerByNight,
      HerdStrategy.EverywhereByDayOwnerByNight,
      manureDepositStategyMilNextYear,
      FertilizerStrategy.UniformFertilizing,
      MulchingStrategy.Mulching(0.10)
    )
    // val kitchenProfile2 = KitchenProfile(10, TwoYears, Parsimonious, AllExtraParcelsLoaner, FoodForAllStrategy)
    //    val kitchenProfile3 = KitchenProfile(10, ThreeYears, Parsimonious, AllExtraParcelsLoaner, FoodForUsOnlyStrategy)
    val kitchenPartition = KitchenPartition((kitchenProfile1, 15), (kitchenProfile2, 5))
    /*, (kitchenProfile2, 16)),(kitchenProfile3, 8)),*/
    val supportPolicy = SupportPolicy(taxPayerRatio = 1, fertilizerWeightPerYear = _ => kitchenPartition.profiles.map(_._2).sum * 20)

    val simulationState = Simulation(
      77,
      giniParcels = 0.1,
      populationGrowth = 0.03,
      kitchenPartition = kitchenPartition,
      supportPolicy = supportPolicy,
      simulationLength = 20,
      hookParameters = hooks)

    println("Pop " + simulationState.populationDynamic)
    println("herd " + simulationState.herdDynamic)
    println("nitrogen " + simulationState.averageNitrogenDynamic)
    println("Soil Quality " + simulationState.averageSoilQualityDynamic)
    println("Average Inexesse " + simulationState.averageInexcessDynamic)
    println("# unbalanced kitchen " + simulationState.numberOfUnbalancedKitchen)
  }


}

