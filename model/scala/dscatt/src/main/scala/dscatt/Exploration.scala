package dscatt

import dscatt.Croping.{Fallow, intToCropZone}
import dscatt.Diohine.HookParameters
import dscatt.Fertility.{fallowFullPotential, fallowNRF}
import dscatt.FoodDonationStrategy.FoodForUsOnlyStrategy
import dscatt.HerdGrazingStrategy.AnywhereAnyTime
import dscatt.LoanStrategy.Selfish
import dscatt.Simulation.SimulationState
import org.apache.commons.math3.stat.regression.SimpleRegression

object Exploration:
  def calibrateCropResidue =

    val manureDepositStategyMilNextYear = { (p: Parcel, r: RotationCycle) =>
      Croping.evolveCrop(p.crop, r, Croping.evolveCropZone(p.cropZone, r)) == Croping.Millet
    }

    val kitchenProfile1 = KitchenProfile(
      kitchenSize = 16,
      RotationCycle.ThreeYears,
      CropingStrategy.PeanutForInexcess(0.0),
      OwnFallowUse.NeverUseFallow,
      LoanStrategy.Selfish,
      FoodDonationStrategy.FoodForUsOnlyStrategy,
      HerdGrazingStrategy.EverywhereByDayOwnerByNight,
      HerdGrazingStrategy.EverywhereByDayOwnerByNight,
      HerdSizeStrategy.NoHerd,
      manureDepositStategyMilNextYear,
      FertilizerStrategy.UniformFertilizing,
      MulchingStrategy.NoMulching,
      0
    )

    val kitchenPartition = KitchenPartition((kitchenProfile1, 1))
    val supportPolicy = SupportPolicy(taxPayerRatio = 1, fertilizerWeightPerYear = _ => kitchenPartition.profiles.map(_._2).sum * 20)
    val hooks = HookParameters(
      displayParcels = false,
      displayKitchens = false,
      hookFile = None
    )

    val world =
      World(
        Seq(
          Parcel(
            id = "1",
            ownerID = 1,
            farmerID = 1,
            crop = Croping.Fallow,
            cropZone = Croping.One,
            area = 1.0,
            faidherbiaTreesByHa = 0.0,
            Seq()
          )
        ),
        1
      )

    val (simulationState: SimulationState, simulationData: Data) = Simulation(
      7L,
      giniParcels = 0.2,
      populationGrowth = 0.014557280935011574,
      kitchenPartition = kitchenPartition,
      supportPolicy = supportPolicy,
      simulationLength = 15,
      soilQualityBasis = 1.0,
      // soilQualityBasis = 0.7468161238013162,
      fallowBoost = 0.0,
      erosion = 1.0,
      sqrf = 1.0,
      peanutSeedToFood = 1.1523627835184518,
      dailyFoodNeedPerPerson = 0.555,
      //      soilQualityBasis = 0.037566386501967745,
      //      fallowBoost = 1.1600945798567732,
      //      peanutSeedToFood = 1.379729887022548,
      //      expandingHerdSize = 1.6991464444068525,
      hookParameters = hooks,
      rainFall = 600,
      //None
      Seq(Switcher(5, SwitchType.Mulching(MulchingStrategy.CropResidue))),
      Some(world)
    )

    given data: Data = simulationData

    println("\nSoil Quality " + simulationState.averageSoilQualityDynamic.toSeq)
    println("\nTotal mulching  " + simulationState.averageMulchingDynamic.toSeq)
    println("\nMil yield dynamic  " + simulationState.averageMilYieldDynamic.toSeq)
