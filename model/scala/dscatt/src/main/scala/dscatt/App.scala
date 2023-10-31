package dscatt

import dscatt.Croping._
import dscatt.KitchenPartition._

object Diohine {

  case class HookFile(outputPath: String, parcels: Boolean, kitchens: Boolean)
  case class HookParameters(displayParcels: Boolean = true, displayKitchens: Boolean = false, hookFile: Option[HookFile] )

  def main(args: Array[String]) = {
    val argOptions = args.lift
    // displayParcels, displayKitchens outputPath parcels kitchens parcelMap
    def toBoolean(s: Option[String]) = s.map{_.toBoolean}.getOrElse(false)
    val hookFile = argOptions(2).map{op=> HookFile(op, toBoolean(argOptions(3)), toBoolean(argOptions(4)))}
    val hooks = HookParameters(toBoolean(argOptions(0)), toBoolean(argOptions(1)), hookFile)

    val manureDepositStategyMilNextYear = { (p: Parcel, r: RotationCycle) =>
      Croping.evolveCrop(p.crop, r, Croping.evolveCropZone(p.cropZone, r)) == Mil
    }

    val kitchenProfile1 = KitchenProfile(
      kitchenSize = 10,
      ThreeYears,
      PeanutForInexcess(0.0),
      NeverUseFallow,
      ExtraParcelsExceptFallowLoaner,
      FoodForAllStrategy,
      herdSize = 10,
      EverywhereByDayOwnerByNight,
      EverywhereByDayOwnerByNight,
      manureDepositStategyMilNextYear,
      UniformFertilizing,
      Mulching(0.10)
    )

    val kitchenProfile2 = KitchenProfile(
      kitchenSize = 10,
      ThreeYears,
      PeanutForInexcess(0.0),
      NeverUseFallow,
      ExtraParcelsExceptFallowLoaner,
      FoodForAllStrategy,
      herdSize = 5,
      EverywhereByDayOwnerByNight,
      EverywhereByDayOwnerByNight,
      manureDepositStategyMilNextYear,
      UniformFertilizing,
      Mulching(0.10)
    )
    // val kitchenProfile2 = KitchenProfile(10, TwoYears, Parsimonious, AllExtraParcelsLoaner, FoodForAllStrategy)
    //    val kitchenProfile3 = KitchenProfile(10, ThreeYears, Parsimonious, AllExtraParcelsLoaner, FoodForUsOnlyStrategy)
    val kitchenPartition = KitchenPartition((kitchenProfile1, 15), (kitchenProfile2, 5))
    /*, (kitchenProfile2, 16)),(kitchenProfile3, 8)),*/
    val supportPolicy = SupportPolicy(taxPayerRatio = 1, fertilizerWeightPerYear = _ => kitchenPartition.profiles.map(_._2).sum * 20)

    Simulation(
        77,
        giniParcels = 0.1,
        populationGrowth = 0.03,
        kitchenPartition = kitchenPartition,
        supportPolicy = supportPolicy,
        simulationLength = 20,
        hookParameters = hooks)
  }

}

