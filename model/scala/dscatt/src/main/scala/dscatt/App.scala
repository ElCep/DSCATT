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
import dscatt.SwitchType.*
import org.apache.commons.math3.stat.regression.SimpleRegression
import better.files._


object Diohine {

  case class HookFile(outputPath: String, parcels: Boolean, kitchens: Boolean, dynamics: Boolean)

  case class HookParameters(displayParcels: Boolean = true, displayKitchens: Boolean = false, hookFile: Option[HookFile])

  def main(args: Array[String])=
    val landsDirectory: better.files.File = java.nio.file.Files.createTempFile("","")
    landsDirectory.overwrite(Geometry.content)

    val seed = 7770

    unitary(seed.toLong, landsDirectory.toJava)

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

  def unitary(seed: Long, lands: java.io.File, kitchenPartition: KitchenPartition = defaultKitchenPartition) = {

    val t1 = System.nanoTime

    val hookFile = HookFile(
      outputPath = "/tmp",
      parcels = false,
      kitchens = false,
      dynamics = true
    )

    val hooks = HookParameters(
      displayParcels = false,
      displayKitchens = true,
      hookFile = None
    )

    /*, (kitchenProfile2, 16)),(kitchenProfile3, 8)),*/
    val supportPolicy = SupportPolicy(taxPayerRatio = 1, fertilizerWeightPerYear = _ => kitchenPartition.profiles.map(_._2).sum * 20)

    val (simulationState, simulationData) = Simulation(
      seed = seed,
      lands = lands,
      populationGrowth = 0.014408809596970397,
      kitchenPartition = kitchenPartition,
      supportPolicy = supportPolicy,
      simulationLength = 26,
      soilQualityBasis = 100,
      fallowBoost = 0.801866457937334,
      cropResidueBoost = 40,
      erosion = 0.01,
      sqrf = 0.019437884479790352,
      peanutSeedToFood = 1.954822292357305,
      dailyFoodNeedPerPerson = 0.555,
      hookParameters = hooks,
      rainFall = Seq(623,623,404,408,388,729,620,528,394,484,395,635,540,526,652,691,720,416,723,536,353,767,527,509,501,501),
      //rainFall = 400,
      Seq()
     //Seq(Switcher(26, SwitchType.Solidarity(Selfish, FoodForUsOnlyStrategy)))
    )

    val duration = (System.nanoTime - t1) / 1e9d

    println("#parc: " + simulationState.world.parcels.length)
    println("#surf: " + simulationState.world.parcels.map(_.area).sum)
    println("Time: " + duration)
    given data: Data = simulationData


//    //    val (rsquare, slope) = simulationState.populationRSquareAndSlope
    println("Pop " + simulationState.populationDynamic.toSeq)
//    //    println("\nPop R2 " + rsquare)
//    //    println("\nPop slope " + slope)
//    println("\nMigrant dynamic  " + simulationState.migrantsDynamic.toSeq)
//    println("Sum of migrants " + simulationState.migrantsDynamic.sum)
//    println("Pop " + simulationState.populationDynamic.last)
    println("\nHerd " + simulationState.herdDynamic.toSeq)
    println("\nNitrogen " + simulationState.averageNitrogenDynamic.toSeq)
//    //    println("\nSoil Quality " + simulationState.averageSoilQualityDynamic.toSeq)
//    //    println("\nAverage Inexesse " + simulationState.averageInexcessDynamic.toSeq)
//    //    println("\n# unbalanced kitchen " + simulationState.numberOfUnbalancedKitchen)
//    println("\nTotal Loaned Area " + simulationState.totalLoanedArea)

    println("\nLoaned dynamics " + simulationState.loanedAreaDynamic.toSeq)
//    //    println("\nManure dynamic " + simulationState.averageManureDynamic.toSeq)
    //println("\nTotal Manure  " + simulationState.totalManure)
//    //    println("\nMulching dynamic " + simulationState.averageMulchingDynamic.toSeq)
//    //    println("\nTotal Mulching  " + simulationState.totalMulching)
//    //    println("\nFFL on Food needs dynamic  " + simulationState.foodFromLoanOnFoodNeedsDynamic.toSeq)
   // println("\nFFD on Food needs dynamic  " + simulationState.foodFromDonationOnFoodNeedsDynamic.toSeq)
//    println("\nKitchen size  " + simulationState.averageKitchenSizeDynamic.toSeq)
//    //    println("\nKSA " + average(simulationState.averageKitchenSizeDynamic.toSeq))
    println("\nEffective fallow " + simulationState.effectiveFallowRatioDynamic.toSeq)
    println("\nFood stress " + simulationState.foodStress.toSeq)
    println("\nMil yield dynamic  " + simulationState.averageMilYieldDynamic.toSeq.length + " :" + simulationState.averageMilYieldDynamic.toSeq)
 //   println("\nMil yield average  " + simulationState.averageMilYieldDynamic.sum / simulationState.averageMilYieldDynamic.length)
//    //    println("\nPeanut yield dynamic  " + simulationState.averagePeanutYieldDynamic.toSeq)
//    //    println("\nNb of kitchens " + simulationState.numberOfKitchens.toSeq)
//    //    val kitchenSoilQuality =
//    //      World.parcelsForKitchen(simulationState.world, Kitchen.kitchen(simulationState.kitchens, 1).get)
//    //      .map(_.fertilityHistory.map(_.agronomicMetrics.soilQuality)).transpose.map(average)
//    //println("SQ for K1 " + kitchenSoilQuality)
//    println("\nSoil Quality " + simulationState.averageSoilQualityDynamic.toSeq)
//    println("NB Absorbed " + simulationState.numberOfAbsorbedKitchens)
    println("\nResidual Soil Quality " + simulationState.averageResidualSoilQualityDynamic.toSeq)

  //  println("Profile Dyn " + simulationState.kitchenProfileRatiosDynamic.toSeq)
    //println("MIL yield " + simulationState.averageMilYieldDynamic.sum / simulationState.averageMilYieldDynamic.length)
   // println(s"$seed, ${simulationState.effectiveFallowRatioDynamic.last},${simulationState.populationDynamic.last},${simulationState.averageMilYieldDynamic.last},${simulationState.herdDynamic.last}")
  }

  def replicate(iterations: Int, landsDirectory: java.io.File) =
    println("seed,ef,pop,yield,herd")
    for i<- 1 to iterations
    do unitary(i.toLong, java.io.File(s"landsDirectory/s{$i}k22g0,20.json"))


}

