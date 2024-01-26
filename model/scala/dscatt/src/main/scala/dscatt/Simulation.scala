package dscatt

import Croping.*
import Diohine.{HookFile, HookParameters}
import History.History
import Kitchen.Food
import org.apache.commons.math3.random.MersenneTwister
import Constants.*

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
             erosion: Double,
             fallowBoost: Double,
             kitchenMinimumSize: Int = 4, // exposed for calibration
             kitchenMaximumSize: Int = 24, // exposed for calibration
             splitKitchenOffspringSize: Int = 6, // exposed for calibration
             peanutSeedToFood: Double, // exposed for calibration
             hookParameters: HookParameters,
             rainFall: MM
           )= {

    given MersenneTwister(seed)
    given rain:MM = rainFall

    val kitchens = Kitchen.buildKitchens(kitchenPartition)

    println("NB KITCH " + kitchens.length)
    println("Area factor " + Constants.AREA_FACTOR)

    val nakedWorld = World.buildWorldGeometry(kitchens, giniParcels)

    println("area totale " + nakedWorld.parcels.map(_.area).sum)
    val initialHistory = History.initialize(simulationLength, kitchens)
    val initialState = SimulationState(nakedWorld, kitchens, initialHistory, 1)

    // Two years warming up
    val warmedUpState = evolve(initialState, 0.0, 3, soilQualityBasis, 0.0, 0.0, false, kitchenMinimumSize, kitchenMaximumSize, splitKitchenOffspringSize, 0.0).copy(history = initialHistory, year = 1)

    println("------------------------------------------------")
    val finalState = evolve(warmedUpState, populationGrowth, simulationLength + 1, soilQualityBasis, erosion, fallowBoost, true, kitchenMinimumSize, kitchenMaximumSize, splitKitchenOffspringSize, peanutSeedToFood)
    
    if (hookParameters.displayParcels)
      History.printParcels(finalState, hookParameters)
    if (hookParameters.displayKitchens)  
      History.printKitckens(finalState, hookParameters)

    finalState
  }


  def evolve(
              simulationState: SimulationState,
              populationGrowth: Double,
              simulationLenght: Int,
              soilQualityBasis: Double,
              erosion: Double,
              fallowBoost: Double,
              emigrationProcess: Boolean,
              kitchenMinimumSize: Int,
              kitchenMaximumSize: Int,
              splitKitchenOffringSize: Int,
              peanutSeedToFood: Double
            )(using MersenneTwister, MM): SimulationState = {

    @tailrec
    def evolve0(simulationState: SimulationState): SimulationState = {
      if (simulationLenght - simulationState.year == 0 || simulationState.kitchens.size <= 1) simulationState
      else {
        val initialFood = simulationState.kitchens.map { k => Food(k.id, -Kitchen.foodNeeds(k)) }

        // Evolve rotation including loans
        val (afterRotationsSimulationState, foodAfterRotation, theoriticalFallowParcels) = Rotation.evolve(simulationState, soilQualityBasis, initialFood)

        // Compute soil quality and available nitrogen for each parcel before the rotation process until fertilities of the current year is computed
        implicit val fertilityMetricsByParcelAfterRotation: Fertility.AgronomicMetricsByParcel = afterRotationsSimulationState.world.parcels.map { p =>
          p.id -> Fertility.agronomicMetrics(p, soilQualityBasis)
        }.toMap

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
        val afterFertilizationState = Fertility.assign(resizedSimulationState, soilQualityBasis)

        val effectiveFallowParcels = World.fallowParcels(afterFertilizationState.world).length

        println("M " + World.milParcels(afterFertilizationState.world.parcels).length + " P " + World.peanutParcels(afterFertilizationState.world.parcels).length + " F " + World.fallowParcels(afterFertilizationState.world).length)
        println("Effective " + effectiveFallowParcels + " Vs Th " + theoriticalFallowParcels )
        val finalHistory = afterFertilizationState.history
          .updateFoods(afterFertilizationState.year, afterDonationFoods)
          .updateEffectiveFallowRatio(afterFertilizationState.year, effectiveFallowParcels.toDouble / theoriticalFallowParcels)

        val finalState = afterFertilizationState.copy(world = Loan.reset(afterFertilizationState.world), year = afterFertilizationState.year + 1, history = finalHistory)

        evolve0(finalState)
      }
    }

    // FIXME: get rid off the var once calibration is done
    Constants.PEANUT_FOOD_EQUIVALENCE = peanutSeedToFood
    Constants.KITCHEN_MINIMUM_SIZE = kitchenMinimumSize
    Constants.KITCHEN_MAXIMUM_SIZE = kitchenMaximumSize
    Constants.SPLIT_KITCHEN_OFFSPRING_SIZE = splitKitchenOffringSize
    Constants.EROSION = erosion
    Constants.FALLOW_BOOST = fallowBoost
    evolve0(simulationState)
  }
}
