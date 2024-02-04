package dscatt

import Croping.*
import Diohine.{HookFile, HookParameters}
import History.History
import Kitchen.Food
import org.apache.commons.math3.random.MersenneTwister
import Data._

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
             peanutSeedToFood: Double, // exposed for calibration
             expandingHerdSize: Double, // exposed for calibration
             hookParameters: HookParameters,
             rainFall: MM
           ) = {
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX " + seed + " " + soilQualityBasis + " " + fallowBoost + " " + peanutSeedToFood + " " + expandingHerdSize)
    given MersenneTwister(seed)

    val data = new Data(
      soilQualityBasis = soilQualityBasis,
      fallowBoost = fallowBoost,
      peanutSeedToFood = peanutSeedToFood,
      expandingHerdSize = expandingHerdSize,
      rainFall = rainFall
    )

    val kitchens = Kitchen.buildKitchens(kitchenPartition)

    println("NB KITCH " + kitchens.length)
    println("Area factor " + data.AREA_FACTOR)

    val nakedWorld = World.buildWorldGeometry(kitchens, giniParcels, data)

    println("area totale " + nakedWorld.parcels.map(_.area).sum)
    val initialHistory = History.initialize(simulationLength, kitchens)
    val initialState = SimulationState(nakedWorld, kitchens, initialHistory, 1)

    // Two years warming up
    val warmedUpState = evolve(initialState, 0.0, 3, false, data).copy(history = initialHistory, year = 1)

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
        val afterDonationFoods = FoodDonation.assign(foodAfterRotation, simulationState)

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
