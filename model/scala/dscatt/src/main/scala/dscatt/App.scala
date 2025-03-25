package dscatt

import Croping.*
import KitchenPartition.*
import utils.*
import Data.*
import dscatt.Fertility.{fallowFullPotential, fallowNRF}
import dscatt.FoodDonationStrategy.FoodForUsOnlyStrategy
import dscatt.HerdGrazingStrategy.AnywhereAnyTime
import dscatt.HerdSizeStrategy.{FullCapacity, LSUByArea}
import dscatt.KitchenComposer.KitchenProfileBuilder
import dscatt.LoanStrategy.Selfish
import dscatt.MulchingStrategy.CropResidue
import dscatt.RotationCycle.MilletPeanut
import dscatt.Simulation.SimulationState
import dscatt.SwitchType.*
import org.apache.commons.math3.stat.regression.SimpleRegression

object Diohine {

  case class HookFile(outputPath: String, parcels: Boolean, kitchens: Boolean, dynamics: Boolean)

  case class HookParameters(displayParcels: Boolean = true, displayKitchens: Boolean = false, hookFile: Option[HookFile])

  def main(args: Array[String])=

//    def checkGrowth =
//      for {
//        g <- (0 to 20 by 1).map(_.toDouble / 1000 + 0.01)
//      } yield {
//        unitary(77L, java.io.File(args.head + "/s777k22g0,20.json"), g)
//      }

    val landsDirectory = java.io.File(args.head + "/s777k22g0,20.json")
   // SwitchExplorer.explore(landsDirectory, "/tmp/newQS")
    //CSVExplorer.run
   // println(RainFallGenerator.thirtyPercentLess.toString + " / " + RainFallGenerator.thirtyPercentLess.size)
    val seed = 7770

//    val kp = KitchenComposer.compose(
//      352,
//      Seq(
//        KitchenProfileBuilder(35, 0.2, 5, 0.42),
//        KitchenProfileBuilder(21, 0.05, 8, 0.42),
//        KitchenProfileBuilder(256, 0.45, 3, 0.0),
//        KitchenProfileBuilder(1101, 0.30, 3, 0.8)
//      )
//    )
//
////    kp.profiles.foreach: p=>
////     println(p._2 + " : " + p._1)
////

   // costCombinatory
    unitary(seed.toLong, landsDirectory)
    //checkGrowth

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

  val defaultKitchenPartition =
    val manureDepositStategyMilNextYear = { (p: Parcel, r: RotationCycle) =>
      Croping.evolveCrop(p.crop, r, Croping.evolveCropZone(p.cropZone, r)) == Millet
    }

    val kitchenProfile1 = KitchenProfile(
      9999,
      kitchenSize = 16,
      RotationCycle.FallowMilletPeanut,
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

    KitchenPartition(Seq((kitchenProfile1, 22)))



  def costCombinatory =
    val social =
      for {
        l <- Seq(2, 9, 10)
        of <- Seq(2,9)
        fd <- Seq(1, 10)
        dhg <- Seq(1, 10)
        whg <- Seq(1, 10)
        hs <- Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        m <- Seq(1,5)
        f <- Seq(1,2,3,4,5)
        pg <- Seq(0,1,2)
      } yield {
        l + of + fd + dhg + whg + hs + m + f + pg
      }

    val mp =
      for {
        l <- Seq(0)
        of <- Seq(0,9)
        fd <- Seq(0,1)
        dhg <- Seq(2,3,9)
        whg <- Seq(3,4,9)
        hs <- Seq(0,1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        m <- Seq(1,8)
        f <- Seq(0, 1,2,3,4,5,6)
        pg <- Seq(5,6,7,8,9,10)
      } yield {
        l + of + fd + dhg + whg + hs + m + f + pg
      }

    val global =
      for {
        s <- social.distinct
        m <- mp.distinct
      } yield {
        s + m
      }

    println(social.distinct)
    println(mp.distinct)
    println(global.distinct)


  def unitary(seed: Long, lands: java.io.File, pg: Double = 0.014, kitchenPartition: KitchenPartition = defaultKitchenPartition) = {

    val t1 = System.nanoTime

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

    /*, (kitchenProfile2, 16)),(kitchenProfile3, 8)),*/
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
      stopCriteria = (simS: SimulationState)=> simS.populationTrend(6,3) < 0,
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



    val popg = simulationState.popStat(26)
    val fertileWomanRatio = 0.5 * 0.2 // half are woman and 20% of woman are 19-34 yo
    val nbFertileWoman = fertileWomanRatio * popg._2
    println(f"Poplation growth: $pg%.3f" + ": " + f"$nbFertileWoman%.2f" + " fertile woman lead to "+ popg._1 + " birth. # child / w: " + (popg._1 / nbFertileWoman))


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
 //   println("\nFood stress " + simulationState.foodStress.toSeq)
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
    println("End simu " + simulationState.year)

  //  println("Profile Dyn " + simulationState.kitchenProfileRatiosDynamic.toSeq)
    //println("MIL yield " + simulationState.averageMilYieldDynamic.sum / simulationState.averageMilYieldDynamic.length)
   // println(s"$seed, ${simulationState.effectiveFallowRatioDynamic.last},${simulationState.populationDynamic.last},${simulationState.averageMilYieldDynamic.last},${simulationState.herdDynamic.last}")
  }

  def replicate(iterations: Int, landsDirectory: java.io.File) =
    println("seed,ef,pop,yield,herd")
    for i<- 1 to iterations
    do unitary(i.toLong, java.io.File(s"landsDirectory/s{$i}k22g0,20.json"))


}

