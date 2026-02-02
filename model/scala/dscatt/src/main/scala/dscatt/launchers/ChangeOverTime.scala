package dscatt.launchers

import dscatt.Simulation.SimulationState
import dscatt.{Croping, CropingStrategy, Data, FertilizerStrategy, FoodDonationStrategy, HerdGrazingStrategy, HerdSizeStrategy, KitchenPartition, KitchenProfile, LoanStrategy, MulchingStrategy, OwnFallowUse, Parcel, RotationCycle, Simulation, SupportPolicy, Switcher}
import dscatt.launchers.commonSettings.*
import dscatt.*

object ChangeOverTime:

  def run(seed: Long, lands: java.io.File, pg: Double = 0.014, kitchenPartition: KitchenPartition = defaultKitchenProfiler.kitchenPartition) =

    val simulationLength = 25

    val manureDepositStategyMilNextYear = { (p: Parcel, r: RotationCycle) =>
      Croping.evolveCrop(p.crop, r, Croping.evolveCropZone(p.cropZone, r)) == Croping.Millet
    }

    val kitchenProfile1 = KitchenProfile(
      1,
      kitchenSize = 16,
      rotationCycle = RotationCycle.FallowMilletPeanut,
      CropingStrategy.PeanutForInexcess(0.0),
      ownFallowUse = OwnFallowUse.UseFallowIfNeeded,
      loanStrategy = LoanStrategy.ExtraParcelsExceptFallowLoaner,
      foodDonationStrategy = FoodDonationStrategy.FoodForAllStrategy,
      drySeasonHerdStrategy = HerdGrazingStrategy.EverywhereByDayOwnerByNight,
      wetSeasonHerdStrategy = HerdGrazingStrategy.EverywhereByDayOwnerByNight,
      herdSizeStrategy = HerdSizeStrategy.LSUByArea(0.42),
      manureDepositStategyMilNextYear,
      FertilizerStrategy.UniformFertilizing,
      MulchingStrategy.NoMulching,
      nbFaidherbia = 4
    )

    val kitchenProfile2 = kitchenProfile1.copy(id = 2)

    val kitchenPartition = KitchenPartition(Seq((kitchenProfile1, 20), (kitchenProfile2, 11)))

    val rnd = scala.util.Random
    val soilCareKP1 = Seq.fill(simulationLength)(rnd.between(0, 16))
    val soilCareKP2 = Seq.fill(simulationLength)(rnd.between(0, 16))
    val switchers =
      Switcher.fromSoilCareScoresToSwitchers(soilCareKP1, 1) ++
        Switcher.fromSoilCareScoresToSwitchers(soilCareKP2, 2)


    val supportPolicy = SupportPolicy(taxPayerRatio = 1, fertilizerWeightPerYear = _ => kitchenPartition.profiles.map(_._2).sum * 20)
    val (simulationState, simulationData) = Simulation(
      seed = seed,
      lands = lands,
      populationGrowth = pg,
      kitchenPartition = kitchenPartition,
      supportPolicy = supportPolicy,
      simulationLength = simulationLength,
      soilQualityBasis = 100,
      fallowBoost = 0.801866457937334,
      cropResidueBoost = 40,
      erosion = 0.01,
      sqrf = 0.019437884479790352,
      peanutSeedToFood = 1.954822292357305,
      dailyFoodNeedPerPerson = 0.555,
      hookParameters = hooks,
      //rainFall = Seq(623,623,404,408,388,729,620,528,394,484,395,635,540,526,652,691,720,416,723,536,353,767,527,509,501,501),
      rainFall = 527,
     // stopCriteria = (simS: SimulationState)=> simS.populationTrend(6,3) < 0,
      stopCriteria = (simS: SimulationState) => false,
      //dumpProfilesPath = Some("/tmp/profiles.json")
      //  Seq(),
      switchers = switchers
    )

    given data: Data = simulationData
    println("Pop " + simulationState.populationDynamic.toSeq)
    println("Ration profile  " + simulationState.proportionOfKitchenProfile(1).toSeq)
    println("MST Count if " + simulationState.mst(simulationState.populationDynamic.map(_.toDouble), (d: Double)=> d >= 31*16))
    println("Mfet " + simulationState.mfet(simulationState.populationDynamic.map(_.toDouble), (d: Double)=> d < 31*16))