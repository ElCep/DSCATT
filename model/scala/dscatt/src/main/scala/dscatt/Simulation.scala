package dscatt

import Croping.*
import Diohine.{HookFile, HookParameters}
import History.History
import Kitchen.{Food, parcelFoodProduction}
import org.apache.commons.math3.random.MersenneTwister
import Data.*
import Parcel.*

import scala.annotation.tailrec

object Simulation {

  case class SimulationState(world: World, kitchens: Seq[Kitchen], history: History, year: Int)

  implicit class SimulationStateWrap(sS: SimulationState):
    def population = History.historyByYear(sS).map(_.population)

    def herds = History.historyByYear(sS).map(_.herds)

    def foodStats = History.historyByYear(sS).map(_.foodStats)

    def parcelStats = History.historyByYear(sS).map(_.parcelStats)

    def fertilityHistory = sS.world.parcels.map(_.fertilityHistory)

    def kitchenProfile = History.historyByYear(sS).map(_.kitchenProfile)

  def apply(
             seed: Long,
             lands: java.io.File,
             populationGrowth: Double,
             kitchenPartition: KitchenPartition = KitchenPartition(Seq((KitchenProfile.default, 1))),
             supportPolicy: SupportPolicy,
             simulationLength: Int = 20,
             soilQualityBasis: SOIL_QUALITY_BY_HA, // exposed for calibration
             fallowBoost: SOIL_QUALITY_BY_HA, // exposed for calibration
             cropResidueBoost: SOIL_QUALITY_BY_HA, // exposed for calibration
             erosion: Double, // exposed for calibration
             sqrf: Double,
             peanutSeedToFood: Double, // exposed for calibration
             dailyFoodNeedPerPerson: Double,
             hookParameters: HookParameters,
             rainFall: Int | MM_PER_YEAR,
             switchers: Seq[Switcher] = Seq(),
             world: Option[World] = None
           ) = {
    given MersenneTwister(seed)

    val normalizedRainFall =
      rainFall match
        case average: Int=> Seq.fill(simulationLength)(average)
        case mmpy: MM_PER_YEAR=>
          assert(mmpy.length == simulationLength)
          mmpy

    val data = new Data(
      soilQualityBasis = soilQualityBasis,
      fallowBoost = fallowBoost,
      cropResidueBoost = cropResidueBoost,
      erosion = erosion,
      sqrf = sqrf,
      peanutSeedToFood = peanutSeedToFood,
      dailyFoodNeedPerPerson = dailyFoodNeedPerPerson,
      rainFall = normalizedRainFall,
      populationGrowth = populationGrowth
    )


    val kitchens = Kitchen.buildKitchens(kitchenPartition)

    val nakedWorld = world.getOrElse(World.buildWorldGeometry(kitchens, lands, data))

    val initialHistory = History.initialize(simulationLength, kitchens)
    val initialState = SimulationState(nakedWorld, kitchens, initialHistory, 1)

    // Two years warming up
    val warmedUpState = evolve(initialState, 0.0, 3, false, data)
      .copy(history = initialHistory,
        year = 1,
        world = initialState.world.copy(parcels = initialState.world.parcels.map(_.resetFertilityHistory))
      )

    val finalState = evolve(warmedUpState, populationGrowth, simulationLength + 1, true, data, switchers)

    if (hookParameters.displayParcels)
      History.printParcels(finalState, hookParameters, data)
    if (hookParameters.displayKitchens)
      History.printKitckens(finalState, hookParameters)

    (finalState, data)
  }

  @tailrec
  def applySwitchers(switchers: Seq[Switcher], simulationState: SimulationState, data: Data): (SimulationState, Data) =
    if (switchers.isEmpty) (simulationState, data)
    else 
      val (newSS, newData) = simulationState.enventuallySwitch(switchers.head, data)
      applySwitchers(switchers.tail, newSS, newData)
      

  def evolve(
              simulationState: SimulationState,
              populationGrowth: Double,
              simulationLenght: Int,
              emigrationProcess: Boolean,
              data: Data,
              switchers: Seq[Switcher] = Seq()
            )(using MersenneTwister): SimulationState = {

    var tour = 0
    @tailrec
    def evolve0(simulationState: SimulationState, data: Data): SimulationState = {
      if (simulationLenght - simulationState.year == 0 || simulationState.kitchens.size < 1) simulationState
      else {
        tour = tour + 1
        println("TOURE " + tour)
        val (switchedSimulationState, switchedData) = applySwitchers(switchers, simulationState, data)

        val initialFood = simulationState.kitchens.map { k => Food(k.id, -Kitchen.foodNeeds(k, switchedData)) }

        // Evolve rotation including loans
        val (afterRotationsSimulationState, foodAfterRotation, theoriticalFallowParcels) = Rotation.evolve(switchedSimulationState, initialFood, switchedData)

        // Process food donations
        val afterDonationFoods = FoodDonation.assign(foodAfterRotation, afterRotationsSimulationState)

        // Process kitchen dynamics (population, emmigrants, absorptions, splits)
        val resizedSimulationState = Kitchen.evolve(
          afterRotationsSimulationState,
          afterDonationFoods,
          emigrationProcess,
          switchedData
        )

        // Process Fertiliy
        val afterFertilizationState = Fertility.assign(resizedSimulationState, switchedData)

        val effectiveFallowParcels =
          val fp = World.fallowParcels(afterFertilizationState.world).length.toDouble
          if (fp.isNaN) 0.0
          else fp

        val finalHistory = afterFertilizationState.history
          .updateFoods(afterFertilizationState.year, afterDonationFoods)
          .updateEffectiveFallowRatio(afterFertilizationState.year, effectiveFallowParcels / theoriticalFallowParcels)

        val finalState = afterFertilizationState.copy(world = Loan.reset(afterFertilizationState.world), year = afterFertilizationState.year + 1, history = finalHistory)

        evolve0(finalState, switchedData)
      }
    }

    evolve0(simulationState, data)
  }
}
