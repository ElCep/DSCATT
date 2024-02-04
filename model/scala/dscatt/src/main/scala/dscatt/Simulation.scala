package dscatt

import Croping.*
import Diohine.{HookFile, HookParameters}
import History.History
import Kitchen.Food
import Constants.*
import org.apache.commons.math3.random.MersenneTwister

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
             soilQualityBasis: Double,
             fallowBoost: Double,
             peanutSeedToFood: Double, // exposed for calibration
             hookParameters: HookParameters,
             rainFall: MM
           )= {

    given MersenneTwister(seed)

    // FIXME: get rid off the var once calibration is done
    Constants.PEANUT_FOOD_EQUIVALENCE = peanutSeedToFood
    Constants.FALLOW_BOOST = fallowBoost
    Constants.SOIL_QUALITY_BASIS = soilQualityBasis

    val kitchens = Kitchen.buildKitchens(kitchenPartition)

    println("NB KITCH " + kitchens.length)
    println("Area factor " + Constants.AREA_FACTOR)

    val nakedWorld = World.buildWorldGeometry(kitchens, giniParcels)

    println("area totale " + nakedWorld.parcels.map(_.area).sum)
    val initialHistory = History.initialize(simulationLength, kitchens)
    val initialState = SimulationState(nakedWorld, kitchens, initialHistory, 1)

    // Two years warming up
    val warmedUpState = evolve(initialState, 0.0, 3, false, rainFall).copy(history = initialHistory, year = 1)

    println("------------------------------------------------ " )
    val finalState = evolve(warmedUpState, populationGrowth, simulationLength + 1, true, rainFall)
    
    if (hookParameters.displayParcels)
      History.printParcels(finalState, hookParameters, rainFall)
    if (hookParameters.displayKitchens)  
      History.printKitckens(finalState, hookParameters)

    finalState
  }


  def evolve(
              simulationState: SimulationState,
              populationGrowth: Double,
              simulationLenght: Int,
              emigrationProcess: Boolean,
              rainFall: MM
            )(using MersenneTwister): SimulationState = {

    @tailrec
    def evolve0(simulationState: SimulationState): SimulationState = {
      if (simulationLenght - simulationState.year == 0 || simulationState.kitchens.size <= 1) simulationState
      else {
        val initialFood = simulationState.kitchens.map { k => Food(k.id, -Kitchen.foodNeeds(k)) }
        
        // Evolve rotation including loans
        val (afterRotationsSimulationState, foodAfterRotation, theoriticalFallowParcels) = Rotation.evolve(simulationState, initialFood, rainFall)
        
        // Process food donations
        val afterDonationFoods = FoodDonation.assign(foodAfterRotation, simulationState)

        // Process kitchen dynamics (population, emmigrants, absorptions, splits)
        val resizedSimulationState = Kitchen.evolve(
          afterRotationsSimulationState,
          populationGrowth,
          afterDonationFoods,
          emigrationProcess
        )
       
        // Process Fertiliy
        val afterFertilizationState = Fertility.assign(resizedSimulationState, rainFall)

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
