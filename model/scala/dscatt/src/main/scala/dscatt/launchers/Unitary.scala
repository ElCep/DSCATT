package dscatt.launchers

import dscatt.launchers.commonSettings.*
import dscatt.Simulation.SimulationState
import dscatt.*

object Unitary:

  def run(seed: Long, lands: java.io.File, pg: Double = 0.014, kitchenPartition: KitchenPartition = defaultKitchenProfiler.kitchenPartition) = {

    val t1 = System.nanoTime

    val kitchenProfiler = defaultKitchenProfiler
    val kitchenPartition = kitchenProfiler.kitchenPartition
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
      rainFall = 527,
      //  stopCriteria = (simS: SimulationState)=> simS.populationTrend(6,3) < 0,
      stopCriteria = (simS: SimulationState) => simS.year >= 3 && (simS.effectiveFallowRatioDynamic.last < 0.5 || simS.foodStress.last < 0.95),
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


    val kitchenProfiles = KitchenProfiler.toJsonContent(kitchenPartition)

    val popg = simulationState.popStat(26)
    val fertileWomanRatio = 0.5 * 0.2 // half are woman and 20% of woman are 19-34 yo
    val nbFertileWoman = fertileWomanRatio * popg._2
    println(f"Poplation growth: $pg%.3f" + ": " + f"$nbFertileWoman%.2f" + " fertile woman lead to " + popg._1 + " birth. # child / w: " + (popg._1 / nbFertileWoman))


    println("soil care score: " + kitchenProfiler.soilCareScore)
    //    //    println("\nPop R2 " + rsquare)
    //   println("\nPop slope " + slope)
    //    println("\nMigrant dynamic  " + simulationState.migrantsDynamic.toSeq)
    //    println("Sum of migrants " + simulationState.migrantsDynamic.sum)
    //    println("Pop " + simulationState.populationDynamic.last)
    //  println("\nherd " + simulationState.herdDynamic.toSeq)
    //   println("\nnitrogen " + simulationState.averageNitrogenDynamic.toSeq)
    //    //    println("\nSoil Quality " + simulationState.averageSoilQualityDynamic.toSeq)
    //    //    println("\nAverage Inexesse " + simulationState.averageInexcessDynamic.toSeq)
    //    //    println("\n# unbalanced kitchen " + simulationState.numberOfUnbalancedKitchen)
    //    println("\nTotal Loaned Area " + simulationState.totalLoanedArea)

    // println("LOaned dynamics " + simulationState.loanedAreaDynamic.toSeq)
    //    //    println("\nManure dynamic " + simulationState.averageManureDynamic.toSeq)
    //println("\nTotal Manure  " + simulationState.totalManure)
    //    //    println("\nMulching dynamic " + simulationState.averageMulchingDynamic.toSeq)
    //    //    println("\nTotal Mulching  " + simulationState.totalMulching)
    //    //    println("\nFFL on Food needs dynamic  " + simulationState.foodFromLoanOnFoodNeedsDynamic.toSeq)
    // println("\nFFD on Food needs dynamic  " + simulationState.foodFromDonationOnFoodNeedsDynamic.toSeq)
    //    println("\nKitchen size  " + simulationState.averageKitchenSizeDynamic.toSeq)
    //    //    println("\nKSA " + average(simulationState.averageKitchenSizeDynamic.toSeq))
    //   println("\nEffective fallow " + simulationState.effectiveFallowRatioDynamic.toSeq)
    println("\nFood stress " + simulationState.foodStress.toSeq)
    //   println("\nMil yield dynamic  " + simulationState.averageMilYieldDynamic.toSeq.length + " :" + simulationState.averageMilYieldDynamic.toSeq)
    //   println("\nMil yield average  " + simulationState.averageMilYieldDynamic.sum / simulationState.averageMilYieldDynamic.length)
    //    //    println("\nPeanut yield dynamic  " + simulationState.averagePeanutYieldDynamic.toSeq)
    //    //    println("\nNb of kitchens " + simulationState.numberOfKitchens.toSeq)
    //    //    val kitchenSoilQuality =
    //    //      World.parcelsForKitchen(simulationState.world, Kitchen.kitchen(simulationState.kitchens, 1).get)
    //    //      .map(_.fertilityHistory.map(_.agronomicMetrics.soilQuality)).transpose.map(average)
    //    //println("SQ for K1 " + kitchenSoilQuality)
    //    println("\nSoil Quality " + simulationState.averageSoilQualityDynamic.toSeq)
    //    println("NB Absorbed " + simulationState.numberOfAbsorbedKitchens)
    //   println("ASQ " + simulationState.averageAnnualSoilQualityDynamic.toSeq)
    //  println("RSQ " + simulationState.averageResidualSoilQualityDynamic.toSeq)
    println("manpower cost " + simulationState.manpowerEffort(pg))
    println("Social cost " + simulationState.socialEffort(pg))
    println("Last food stress " + simulationState.foodStress.last)
    println("Last Effective Fallow ratio " + simulationState.effectiveFallowRatioDynamic.last)
    println("End simu " + simulationState.year)

    //  println("Profile Dyn " + simulationState.kitchenProfileRatiosDynamic.toSeq)
    //println("MIL yield " + simulationState.averageMilYieldDynamic.sum / simulationState.averageMilYieldDynamic.length)
    // println(s"$seed, ${simulationState.effectiveFallowRatioDynamic.last},${simulationState.populationDynamic.last},${simulationState.averageMilYieldDynamic.last},${simulationState.herdDynamic.last}")
  }