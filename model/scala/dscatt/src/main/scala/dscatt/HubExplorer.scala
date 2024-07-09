package dscatt

import better.files.File
import dscatt.Diohine.{HookFile, HookParameters}
import dscatt.Fertility.{fallowFullPotential, fallowNRF}
import dscatt.FoodDonationStrategy.FoodForUsOnlyStrategy
import dscatt.HerdGrazingStrategy.AnywhereAnyTime
import dscatt.HerdSizeStrategy.{FullCapacity, LSUByArea}
import dscatt.LoanStrategy.Selfish
import dscatt.{Croping, CropingStrategy, Data, FertilizerStrategy, FoodDonationStrategy, HerdGrazingStrategy, HerdSizeStrategy, KitchenPartition, KitchenProfile, LoanStrategy, MulchingStrategy, OwnFallowUse, Parcel, RotationCycle, Simulation, SupportPolicy, SwitchType, Switcher, utils}
import dscatt.MulchingStrategy.CropResidue
import dscatt.RotationCycle.TwoYears
import dscatt.Simulation.SimulationState
import dscatt.SwitchType.*
import org.apache.commons.math3.stat.regression.SimpleRegression

// Apply p switchers among n available switchers in one simulation
object HubExplorer:

  def explore(switchTime: Int,
              rainfall: RainFall,
              faidherbia: Faidherbia,
              loan: Loan,
              foodDonation: FoodDonation,
              rotation: Rotation,
              dryGrazing: DryGrazing,
              wetGrazing: WetGrazing,
              herdSize: HerdSize,
              mulching: Mulching,
              demography: Demography,
              peanutSeedToFood: PeanutSeedToFood,
              peanutForInexcess: PeanutInexcess
             ) =

    val switchers = Seq(rainfall, faidherbia, loan, foodDonation, rotation, peanutForInexcess, dryGrazing, wetGrazing,
      herdSize, mulching, demography, peanutSeedToFood, peanutForInexcess).map(st => Switcher(switchTime, st))

    val manureDepositStategyMilNextYear = { (p: Parcel, r: RotationCycle) =>
      Croping.evolveCrop(p.crop, r, Croping.evolveCropZone(p.cropZone, r)) == Croping.Millet
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

    val hooks = HookParameters(
      displayParcels = false,
      displayKitchens = false,
      hookFile = None
    )


    println("Running ")
    switchers.foreach(println)
    val (simulationState: SimulationState, simulationData: Data) = Simulation(
      7L,
      giniParcels = 0.2,
      populationGrowth = 0.013222486737573532,
      kitchenPartition = kitchenPartition,
      supportPolicy = supportPolicy,
      simulationLength = 50,
      soilQualityBasis = 100,
      fallowBoost = 1.8226887818701696,
      cropResidueBoost = 0.0,
      erosion = 0.02,
      sqrf = 0.02162343712017012,
      peanutSeedToFood = 1.1264894749651675,
      dailyFoodNeedPerPerson = 0.555,
      hookParameters = hooks,
      rainFall = 600,
      switchers
    )

    println("Pop " + simulationState.populationDynamic.toSeq)
    println("\nMigrant dynamic  " + simulationState.migrantsDynamic.toSeq)
    println("Sum of migrants " + simulationState.migrantsDynamic.sum)
    println("Pop " + simulationState.populationDynamic.last)
    println("\nherd " + simulationState.herdDynamic.toSeq)
    println("\nnitrogen " + simulationState.averageNitrogenDynamic.toSeq)
    println("\nKitchen size  " + simulationState.averageKitchenSizeDynamic.toSeq)
    println("\nEffective fallow " + simulationState.effectiveFallowRatioDynamic.toSeq)
    println("\nFood stress " + simulationState.foodStress.toSeq)
    println("\nMil yield dynamic  " + simulationState.averageMilYieldDynamic.toSeq)
    println("\nSoil Quality " + simulationState.averageAnnualSoilQualityDynamic.toSeq)
    println("\nSoil Quality " + simulationState.averageResidualSoilQualityDynamic.toSeq)
    println("NB Absorbed " + simulationState.numberOfAbsorbedKitchens)
    println("SQ x N " + simulationState.averageSQByNitrogenDynamic.toSeq)


