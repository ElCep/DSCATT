package dscatt

import Croping.*
import KitchenPartition.*
import utils.*
import Data.*
import dscatt.Fertility.{fallowFullPotential, fallowNRF}
import dscatt.FoodDonationStrategy.FoodForUsOnlyStrategy
import dscatt.HerdGrazingStrategy.AnywhereAnyTime
import dscatt.HerdSizeStrategy.{FullCapacity, LSUByArea}
import dscatt.LoanStrategy.Selfish
import dscatt.MulchingStrategy.CropResidue
import dscatt.RotationCycle.TwoYears
import dscatt.SwitchType.*
import org.apache.commons.math3.stat.regression.SimpleRegression

object Diohine {

  case class HookFile(outputPath: String, parcels: Boolean, kitchens: Boolean, dynamics: Boolean)

  case class HookParameters(displayParcels: Boolean = true, displayKitchens: Boolean = false, hookFile: Option[HookFile])

  def main(args: Array[String])=
    val landsDirectory = java.io.File(args.head + "/s777k22g0,20.json")
    SwitchExplorer.explore(landsDirectory, "/tmp/newQS")
   // CSVExplorer.run
    val seed = 777
  //  unitary(seed.toLong, java.io.File(landsDirectory, s"s${seed}k22g0,20.json"))
   // replicate(1000, landsDirectory)

//    HubExplorer.explore(
//      switchTime = 26,
//      rainfall = RainFall(700),
//      faidherbia = Faidherbia(6),
//      loan = Loan(LoanStrategy.AllExtraParcelsLoaner),
//      foodDonation = FoodDonation(FoodDonationStrategy.FoodForUsOnlyStrategy),
//      rotation = Rotation(RotationCycle.ThreeYears),
//      dryGrazing = DryGrazing(HerdGrazingStrategy.EverywhereByDayOwnerByNight),
//      wetGrazing = WetGrazing(HerdGrazingStrategy.AnywhereAnyTime),
//      herdSize = HerdSize(HerdSizeStrategy.LSUByArea(0.6)),
//      mulching = Mulching(CropResidue(0.3)),
//      demography = Demography(0.010),
//      peanutSeedToFood = PeanutSeedToFood(1.3),
//      peanutForInexcess = PeanutInexcess(0.1)
//    )

  def unitary(seed: Long, lands: java.io.File) = {

    val hookFile = HookFile(
      outputPath = "/tmp",
      parcels = false,
      kitchens = false,
      dynamics = true
    )

    val hooks = HookParameters(
      displayParcels = false,
      displayKitchens = false,
      hookFile = None
    )

    val manureDepositStategyMilNextYear = { (p: Parcel, r: RotationCycle) =>
      Croping.evolveCrop(p.crop, r, Croping.evolveCropZone(p.cropZone, r)) == Millet
    }

    val kitchenProfile1 = KitchenProfile(
      kitchenSize = 16,
      RotationCycle.ThreeYears,
      CropingStrategy.PeanutForInexcess(0.0),
      OwnFallowUse.NeverUseFallow,
      LoanStrategy.AllExtraParcelsLoaner,
      FoodDonationStrategy.FoodForAllStrategy,
      HerdGrazingStrategy.EverywhereByDayOwnerByNight,
      HerdGrazingStrategy.EverywhereByDayOwnerByNight,
      HerdSizeStrategy.LSUByArea(0.42), // = 0.42, // in [0.0; 0.68] 0.68 is more or less equivalent to 140 LSU, which is a maximum possible for the whole area
      manureDepositStategyMilNextYear,
      FertilizerStrategy.UniformFertilizing,
      MulchingStrategy.NoMulching,
      4
    )

    val kitchenPartition = KitchenPartition((kitchenProfile1, 22))
    /*, (kitchenProfile2, 16)),(kitchenProfile3, 8)),*/
    val supportPolicy = SupportPolicy(taxPayerRatio = 1, fertilizerWeightPerYear = _ => kitchenPartition.profiles.map(_._2).sum * 20)

    val (simulationState, simulationData) = Simulation(
      seed = seed,
      lands = lands,
      populationGrowth = 0.014488068822213016,
      kitchenPartition = kitchenPartition,
      supportPolicy = supportPolicy,
      simulationLength = 26,
      soilQualityBasis = 100,
      fallowBoost = 2.505042416468803,
      cropResidueBoost = 37,
      erosion = 0.001,
      sqrf = 0.015458790627221223,
      peanutSeedToFood = 1.5831974550765018,
      dailyFoodNeedPerPerson = 0.555,
      hookParameters = hooks,
      rainFall = 600,
      Seq()
     //Seq(Switcher(26, SwitchType.Solidarity(Selfish, FoodForUsOnlyStrategy)))
    )

    given data: Data = simulationData

//    //    val (rsquare, slope) = simulationState.populationRSquareAndSlope
//    println("Pop " + simulationState.populationDynamic.toSeq)
//    //    println("\nPop R2 " + rsquare)
//    //    println("\nPop slope " + slope)
//    println("\nMigrant dynamic  " + simulationState.migrantsDynamic.toSeq)
//    println("Sum of migrants " + simulationState.migrantsDynamic.sum)
//    println("Pop " + simulationState.populationDynamic.last)
//    println("\nherd " + simulationState.herdDynamic.toSeq)
//    println("\nnitrogen " + simulationState.averageNitrogenDynamic.toSeq)
//    //    println("\nSoil Quality " + simulationState.averageSoilQualityDynamic.toSeq)
//    //    println("\nAverage Inexesse " + simulationState.averageInexcessDynamic.toSeq)
//    //    println("\n# unbalanced kitchen " + simulationState.numberOfUnbalancedKitchen)
//    //    println("\nTotal Loaned Area " + simulationState.totalLoanedArea)
//    //    println("\nManure dynamic " + simulationState.averageManureDynamic.toSeq)
//    //    println("\nTotal Manure  " + simulationState.totalManure)
//    //    println("\nMulching dynamic " + simulationState.averageMulchingDynamic.toSeq)
//    //    println("\nTotal Mulching  " + simulationState.totalMulching)
//    //    println("\nFFL on Food needs dynamic  " + simulationState.foodFromLoanOnFoodNeedsDynamic.toSeq)
//    //    println("\nFFD on Food needs dynamic  " + simulationState.foodFromDonationOnFoodNeedsDynamic.toSeq)
//    println("\nKitchen size  " + simulationState.averageKitchenSizeDynamic.toSeq)
//    //    println("\nKSA " + average(simulationState.averageKitchenSizeDynamic.toSeq))
//    println("\nEffective fallow " + simulationState.effectiveFallowRatioDynamic.toSeq)
//    println("\nFood stress " + simulationState.foodStress.toSeq)
  //  println("\nMil yield dynamic  " + simulationState.averageMilYieldDynamic.toSeq)
//    //    println("\nPeanut yield dynamic  " + simulationState.averagePeanutYieldDynamic.toSeq)
//    //    println("\nNb of kitchens " + simulationState.numberOfKitchens.toSeq)
//    //    val kitchenSoilQuality =
//    //      World.parcelsForKitchen(simulationState.world, Kitchen.kitchen(simulationState.kitchens, 1).get)
//    //      .map(_.fertilityHistory.map(_.agronomicMetrics.soilQuality)).transpose.map(average)
//    //println("SQ for K1 " + kitchenSoilQuality)
//    println("\nSoil Quality " + simulationState.averageSoilQualityDynamic.toSeq)
//    println("NB Absorbed " + simulationState.numberOfAbsorbedKitchens)
  //  println("SQ " + simulationState.averageAnnualSoilQualityDynamic.toSeq)

    //println("MIL yield " + simulationState.averageMilYieldDynamic.sum / simulationState.averageMilYieldDynamic.length)
    println(s"$seed, ${simulationState.effectiveFallowRatioDynamic.last},${simulationState.populationDynamic.last},${simulationState.averageMilYieldDynamic.last},${simulationState.herdDynamic.last}")
  }

  def replicate(iterations: Int, landsDirectory: java.io.File) =
    println("seed,ef,pop,yield,herd")
    for i<- 1 to iterations
    do unitary(i.toLong, java.io.File(s"landsDirectory/s{$i}k22g0,20.json"))


}

