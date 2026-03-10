package dscatt.launchers

import dscatt.Simulation.SimulationState
import dscatt.{Croping, CropingStrategy, Data, FertilizerStrategy, FoodDonationStrategy, HerdGrazingStrategy, HerdSizeStrategy, KitchenPartition, KitchenProfile, LoanStrategy, MulchingStrategy, OwnFallowUse, Parcel, RotationCycle, Simulation, SupportPolicy, Switcher}
import dscatt.launchers.commonSettings.*
import dscatt.*
import Croping.Crop.*
import dscatt.HerdSizeStrategy.LSUByArea

import scala.annotation.tailrec

object ChangeOverTime:

  def run(seed: Long, lands: java.io.File, pg: Double = 0.014, kitchenPartition: KitchenPartition = defaultKitchenProfiler.kitchenPartition) =

    val simulationLength = 100

    val manureDepositStategyMilNextYear = { (p: Parcel, r: RotationCycle) =>
      Croping.nextCrop(r, p.crop) == Some(Millet)
     // Croping.evolveCrop(p.crop, r, Croping.evolveCropZone(p.cropZone, r)) == Croping.Millet
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

    val kitchenPartition = KitchenPartition(Seq((kitchenProfile1, 21), (kitchenProfile2, 10)))

    val rnd = scala.util.Random

    def expandScores(sixYearScores: Seq[Int]) =
      sixYearScores.map(x => Seq.fill(6)(x)).reduce(_ ++ _).dropRight(2)
      
    def expandHerdSize(sixYearScores: Seq[HerdSizeStrategy]) =
      sixYearScores.map(x => Seq.fill(6)(x)).reduce(_ ++ _).dropRight(2)
    
    val soilCareKP1 = expandScores(Array(15,13,11,11,11,9,9,7,7,9,5,7,7,5,5,5,7))
    val soilCareKP2 = expandScores(Array(10,14,8,12,2,6,4,8,4,10,10,0,10,0,12,14,4))
   // val herdSizeStrategyKP1 = expandHerdSize(Array.fill(10)(HerdSizeStrategy.FullCapacity) ++ Array.fill(7)(HerdSizeStrategy.NoHerd)))
   // val herdSizeStrategyKP2 = expandHerdSize(Array.fill(17)(LSUByArea(0.42)))
  //  val soilCareKP2 = Array.fill(24)(12)// Array(1,2,13,7,7,7,2,3,7,9,10,2,3,7,7,7,2,8,7,7,1,7,7,7,7)//Array.fill(24)(12) //Seq.fill(simulationLength)(rnd.between(0, 16))
//      val herdGrazingScoreKP1 = Array(1,2,2,1,2,2,5,8,1,1,1,8,8,2,4,4,4,0,0,0,0,0,1,1)
//      val herdGrazingScoreKP2 = Array(2,2,5,8,1,1,1,8,8,2,4,4,4,0,0,0,0,0,1,1,5,5,2,3)

    @tailrec def nbSwitches(a: Array[Int], nbS: Int): Int =
      if a.isEmpty
      then nbS - 1
      else {
        val first = a.head
        val size = a.takeWhile(_ == first).length
        nbSwitches(a.drop(size), nbS + 1)
      }

//    val switchers =
//      Switcher.fromSoilCareScoresToSwitchers(soilCareKP1, 1) ++
//        Switcher.fromSoilCareScoresToSwitchers(soilCareKP2, 2)

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
      //rainFall = Seq(623,623,404,408,388,729,620,528,394,484,395,635,540,526,652,691,720,416,723,536,353,767,527,509,501),
      rainFall = Seq(
        498,326,578,496,400,317,498,498,613,496,521,552,387,387,508,420,326,578,326,508,326,326,428,420,420,317,613,576,578,498,326,578,498,508,
        583,317,583,552,508,498,552,326,421,326,498,323,317,576,420,521,317,428,498,508,613,317,407,496,422,613,613,613,428,400,428,317,508,332,
        498,498,498,552,323,317,498,420,432,508,387,498,428,496,552,317,407,387,332,498,317,422,613,317,576,428,387,332,332,420,496,387
      ),
      //rainFall = 527,
     // stopCriteria = (simS: SimulationState)=> simS.populationTrend(6,3) < 0,
      stopCriteria = (simS: SimulationState) => false,
      //dumpProfilesPath = Some("/tmp/profiles.json")
      //  Seq(),
      switchers = switchers
      //switchers = Seq()Seq(498,498,323,326,317,583,496,422,317,387,317,508,432,420,521,552,576,332,578,428,317,613,421,407,400),
    )

    given data: Data = simulationData
    println("Pop " + simulationState.populationDynamic.toSeq)
    println("Ration profile  " + simulationState.proportionOfKitchenProfile(1).toSeq)
    println("MST Count if " + simulationState.mst(simulationState.populationDynamic.map(_.toDouble), (d: Double)=> d >= 31*16))
    println("Mfet " + simulationState.mfet(simulationState.populationDynamic.map(_.toDouble), (d: Double)=> d < 31*16))
    println("Soil Care social effort " )
    println(simulationState.sumOfSoilCareSocialEfforts)
    println(simulationState.migrantsDynamic.toSeq.sum)
    println("\nMil yield dynamic  " + simulationState.averageMilYieldDynamic.toSeq)
    println("\nPOP dynamic  " + simulationState.populationDynamic.toSeq)
    println("\nMIG dynamic  " + simulationState.migrantsDynamic.toSeq)
    println("\nFallow dynamic  " + simulationState.effectiveFallowRatioDynamic.toSeq)


    println("Produced from KP1 " + simulationState.producedManureFrom(1, data).toSeq + " :: " + simulationState.producedManureFrom(1, data).size)
    println("Manure Stream KP1 " + simulationState.manureStreamLogRatio(1, data))

    println("-----")
    println("Manure Stream KP2 " + simulationState.manureStreamLogRatio(2, data))

    println("----------")
    println(" loans from 1 to 2 " + simulationState.foodFromloansFrom(1, data))
    println(" loans from 2 to 1 " + simulationState.foodFromloansFrom(2, data))


    println("----------")
    println(" donation from 1 to 2 " + simulationState.foodDonationFrom(1, data))
    println(" donaiton from 2 to 1 " + simulationState.foodDonationFrom(2, data))

    println("------------")

    println(" solidarity food from 1 to 2 " + simulationState.solidarityFoodFrom(1, data))
    println(" solidarity food from 2 to 1 " + simulationState.solidarityFoodFrom(2, data))

    println("Mig metric " + simulationState.migrantsDynamic.max  + ", " +  simulationState.migrantsDynamic.count(_ != 0.0) + " => " + simulationState.migrantsDynamic.max * simulationState.migrantsDynamic.count(_ != 0.0))

