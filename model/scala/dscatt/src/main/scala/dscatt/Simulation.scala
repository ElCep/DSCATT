package dscatt

import Croping.*
import Diohine.{HookFile, HookParameters}
import History.History
import Kitchen.Food
import org.apache.commons.math3.random.MersenneTwister
import Data._
import Parcel._

import scala.annotation.tailrec

object Simulation {

  case class SimulationState(world: World, kitchens: Seq[Kitchen], history: History, year: Int)

  implicit class SimulationStateWrap(sS: SimulationState):
    def population = History.historyByYear(sS).map(_.population)

    def herds = History.historyByYear(sS).map(_.herds)

    def foodStats = History.historyByYear(sS).map(_.foodStats)

    def parcelStats = History.historyByYear(sS).map(_.parcelStats)

    def fertilityHistory = sS.world.parcels.map(_.fertilityHistory)

  def apply(
             seed: Long,
             giniParcels: Double,
             populationGrowth: Double,
             kitchenPartition: KitchenPartition = KitchenPartition((KitchenProfile.default, 1)),
             supportPolicy: SupportPolicy,
             simulationLength: Int = 20,
             soilQualityBasis: Double, // exposed for calibration
             fallowBoost: Double, // exposed for calibration
             erosion: Double, // exposed for calibration
             peanutSeedToFood: Double, // exposed for calibration
             expandingHerdSize: Double, // exposed for calibration
             dailyFoodNeedPerPerson: Double,
             hookParameters: HookParameters,
             rainFall: MM
           ) = {
    given MersenneTwister(seed)

    val data = new Data(
      soilQualityBasis = soilQualityBasis,
      fallowBoost = fallowBoost,
      erosion = erosion,
      peanutSeedToFood = peanutSeedToFood,
      expandingHerdSize = expandingHerdSize,
      dailyFoodNeedPerPerson = dailyFoodNeedPerPerson,
      rainFall = rainFall
    )

    val kitchens = Kitchen.buildKitchens(kitchenPartition)

    val nakedWorld = World.buildWorldGeometry(kitchens, giniParcels, data)

    println("Area " + nakedWorld.parcels.map(_.area).sum)
    val initialHistory = History.initialize(simulationLength, kitchens)
    val initialState = SimulationState(nakedWorld, kitchens, initialHistory, 1)

    // Two years warming up
    val warmedUpState = evolve(initialState, 0.0, 3, false, data)
      .copy(history = initialHistory,
        year = 1,
        world = initialState.world.copy(parcels = initialState.world.parcels.map(_.resetFertilityHistory))
      )

    println("------------------------------------------------ ")
    val finalState = evolve(warmedUpState, populationGrowth, simulationLength + 1, true, data)

    if (hookParameters.displayParcels)
      History.printParcels(finalState, hookParameters, data)
    if (hookParameters.displayKitchens)
      History.printKitckens(finalState, hookParameters)

    (finalState, data)
  }


  def evolve(
              simulationState: SimulationState,
              populationGrowth: Double,
              simulationLenght: Int,
              emigrationProcess: Boolean,
              data: Data
            )(using MersenneTwister): SimulationState = {

    @tailrec
    def evolve0(simulationState: SimulationState): SimulationState = {
      if (simulationLenght - simulationState.year == 0 || simulationState.kitchens.size <= 1) simulationState
      else {
        val initialFood = simulationState.kitchens.map { k => Food(k.id, -Kitchen.foodNeeds(k, data)) }

        // Evolve rotation including loans
        val (afterRotationsSimulationState, foodAfterRotation, theoriticalFallowParcels) = Rotation.evolve(simulationState, initialFood, data)

        // Process food donations
        val afterDonationFoods = FoodDonation.assign(foodAfterRotation, afterRotationsSimulationState)

        // Process kitchen dynamics (population, emmigrants, absorptions, splits)
        val resizedSimulationState = Kitchen.evolve(
          afterRotationsSimulationState,
          populationGrowth,
          afterDonationFoods,
          emigrationProcess,
          data
        )

        // Process Fertiliy
        val afterFertilizationState = Fertility.assign(resizedSimulationState, data)

        val effectiveFallowParcels = World.fallowParcels(afterFertilizationState.world).length

        val finalHistory = afterFertilizationState.history
          .updateFoods(afterFertilizationState.year, afterDonationFoods)
          .updateEffectiveFallowRatio(afterFertilizationState.year, effectiveFallowParcels.toDouble / theoriticalFallowParcels)

        val finalState = afterFertilizationState.copy(world = Loan.reset(afterFertilizationState.world), year = afterFertilizationState.year + 1, history = finalHistory)

        evolve0(finalState)
      }
    }

    evolve0(simulationState)
  }
}
