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
import dscatt.OwnFallowUse.UseFallowIfNeeded
import dscatt.RotationCycle.TwoYears
import dscatt.Simulation.SimulationState
import dscatt.SwitchType.{Demography, Faidherbia, HerdSize, Mulching, OwnFallow, RainFall, Rotation, Solidarity}
import org.apache.commons.math3.stat.regression.SimpleRegression

// Apply n switchers in n similations (one switcher per simulation) 
object SwitchExplorer:

  def explore(outputPath: String) =
    val file = File(outputPath)
    file.createDirectories()

    val hookFile = HookFile(
      outputPath = outputPath,
      parcels = false,
      kitchens = false,
      dynamics = true
    )



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
      hookFile = Some(hookFile)
    )

    val switchers =
      Seq(
        Seq(),
        Seq(Switcher(26, RainFall(700))),
        Seq(Switcher(26, Faidherbia(6))),
        Seq(Switcher(26, HerdSize(LSUByArea(0.525)))),
        Seq(Switcher(26, HerdSize(FullCapacity))),
        Seq(Switcher(26, Mulching(CropResidue))),
        Seq(Switcher(26, Rotation(TwoYears))),
        Seq(Switcher(26, SwitchType.Grazing(AnywhereAnyTime, AnywhereAnyTime))),
        //Seq(Switcher(26, Solidarity(Selfish, FoodForUsOnlyStrategy))),
        Seq(Switcher(26, OwnFallow(UseFallowIfNeeded))),
        Seq(Switcher(26, Demography(0.006433183))) // 50%
      )

    val dynamics = switchers.map: s=>
      println("Running " + s)
      val (state: SimulationState, simulationData: Data) = Simulation(
        7L,
        giniParcels = 0.2,
        populationGrowth = 0.014460237572220756,
        kitchenPartition = kitchenPartition,
        supportPolicy = supportPolicy,
        simulationLength = 50,
        soilQualityBasis = 100,
        fallowBoost = 1.996424612178958,
        erosion = 0.02,
        sqrf = 0.02100743042984097,
        peanutSeedToFood = 1.3353399052412946,
        dailyFoodNeedPerPerson = 0.555,
        hookParameters = hooks,
        rainFall = 600,
        s
      )
      
      Seq(
        state.populationDynamic.tail.toSeq,
        state.herdDynamic.tail.toSeq,
        state.averageSoilQualityDynamic.tail.toSeq,
        state.averageNitrogenDynamic.tail.toSeq,
        state.averageSQByNitrogenDynamic.tail.toSeq,
        state.averageMilYieldDynamic.tail.toSeq,
        state.effectiveFallowRatioDynamic.tail.toSeq,
        state.loanedAreaDynamic.tail.toSeq,
        state.foodStress.tail.toSeq
    )
      
    val transposed = dynamics.transpose

    transposed.zip(Seq("pop", "herd", "qs", "nitrogen", "qsXnitrogen", "milletYield", "ef", "loan", "foodStress")).foreach: (metrics, label)=>
      val file = File(s"${outputPath}/${label}.csv")
      val content =
        val headers = Seq("Base", "RainFall","Faidherbia", "LSU +25%", "LSU Full capacity", "Crop residue 50%", "Grazing anywhere anytime", "No loan no food donation", "Use own fallow permitted", "Demography -50%")
        (headers zip metrics.map(_.map(_.toString))).map((h,t)=> h +: t).transpose.map(_.mkString(",")).mkString("\n")


      file.overwrite(content)
