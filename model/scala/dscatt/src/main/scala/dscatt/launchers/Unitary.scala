package dscatt.launchers

import dscatt.launchers.commonSettings.*
import dscatt.Simulation.SimulationState
import dscatt.*

object Unitary:

  def run(seed: Long, lands: java.io.File, pg: Double = 0.014, kitchenPartition: KitchenPartition = defaultKitchenProfiler.kitchenPartition) = {

    val t1 = System.nanoTime

    //val kitchenProfiler = defaultKitchenProfiler


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
      KitchenComposer.manureDepositStategyMilNextYear,
      FertilizerStrategy.UniformFertilizing,
      MulchingStrategy.NoMulching,
      nbFaidherbia = 4
    )

    val kitchenProfile2 = kitchenProfile1.copy(id = 2)

    val kitchenPartition = KitchenPartition(Seq((kitchenProfile1, 21), (kitchenProfile2, 10)))

    //val kitchenPartition = kitchenPartition
    val supportPolicy = SupportPolicy(taxPayerRatio = 1, fertilizerWeightPerYear = _ => kitchenPartition.profiles.map(_._2).sum * 20)

    val (simulationState, simulationData) = Simulation(
      seed = seed,
      lands = lands,
      populationGrowth = pg,
      kitchenPartition = kitchenPartition,
      supportPolicy = supportPolicy,
      simulationLength = 100,
      soilQualityBasis = 100,
      fallowBoost = 0.801866457937334,
      cropResidueBoost = 40,
      erosion = 0.01,
      sqrf = 0.019437884479790352,
      peanutSeedToFood = 1.954822292357305,
      dailyFoodNeedPerPerson = 0.555,
      hookParameters = hooks,
      //rainFall = Seq(623,623,404,408,388,729,620,528,394,484,395,635,540,526,652,691,720,416,723,536,353,767,527,509,501,501),
      //rainFall = 527,
      rainFall = Seq(498,498,323,326,317,583,496,422,317,387,317,508,432,420,521,552,576,332,578,428,317,613,421,407,400) ++ Seq.fill(75)(430)
      //  stopCriteria = (simS: SimulationState)=> simS.populationTrend(6,3) < 0,
      //stopCriteria = (simS: SimulationState) => simS.year >= 3 && (simS.effectiveFallowRatioDynamic.last < 0.5 || simS.foodStress.last < 0.95),
      //dumpProfilesPath = Some("/tmp/profiles.json")
      //  Seq(),
      //Seq(Switcher(26, SwitchType.Solidarity(Selfish, FoodForUsOnlyStrategy)))
    )

    val duration = (System.nanoTime - t1) / 1e9d

    //println("#parc: " + simulationState.world.parcels.length)
    //println("#surf: " + simulationState.world.parcels.map(_.area).sum)
    //println("Time: " + duration)
    given data: Data = simulationData


    //    //    val (rsquare, slope) = simulationState.populationRSquareAndSlope
    //println("Pop " + simulationState.populationDynamic.toSeq)


//    val kitchenProfiles = KitchenProfiler.toJsonContent(kitchenPartition)
//
//    val popg = simulationState.popStat(26)
//    val fertileWomanRatio = 0.5 * 0.2 // half are woman and 20% of woman are 19-34 yo
//    val nbFertileWoman = fertileWomanRatio * popg._2
//    println(f"Poplation growth: $pg%.3f" + ": " + f"$nbFertileWoman%.2f" + " fertile woman lead to " + popg._1 + " birth. # child / w: " + (popg._1 / nbFertileWoman))



//    println("ASQ: " + simulationState.averageAnnualSoilQualityDynamic.toSeq)
//    println("Res: " + simulationState.averageResidualSoilQualityDynamic.toSeq)
    println("E Fallow: " + simulationState.effectiveFallowRatioDynamic.toSeq)
    println("Herd dyn: " + simulationState.herdDynamic.toSeq)
    println("Herds from KP1 " + simulationState.herdsFrom(1).toSeq)
    println("Herds from KP2 " + simulationState.herdsFrom(2).toSeq)
    println("Balance " + simulationState.herdsFrom(1).zip(simulationState.herdsFrom(2)).map(_-_).toSeq)
//    println("Mil yield: " +  simulationState.averageMilYieldDynamic.toSeq)
//    println("Herd dyn: " + simulationState.herdDynamic.toSeq)
//    println("Nitrogen: " + simulationState.averageNitrogenDynamic.toSeq)
//    println("Mig: " +  simulationState.migrantsDynamic.toSeq)
//    println("Pop: " +  simulationState.populationDynamic.toSeq)

  }