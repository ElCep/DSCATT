package dscatt

import Croping.*
import Diohine.{HookFile, HookParameters}
import History.History
import Kitchen.Food
import org.apache.commons.math3.random.MersenneTwister

import scala.annotation.tailrec

object Simulation {

  case class SimulationState(world: World, kitchens: Seq[Kitchen], history: History, year: Int)

  def apply(
             seed: Long,
             giniParcels: Double,
             populationGrowth: Double,
             kitchenPartition: KitchenPartition = KitchenPartition((KitchenProfile.default, 1)),
             supportPolicy: SupportPolicy,
             simulationLength: Int = 20,
             soilQualityBasis: Double = 0.8,
             hookParameters: HookParameters
           ) = {

    given MersenneTwister(seed)

    val kitchens = Kitchen.buildKitchens(kitchenPartition)

    println("NB KITCH " + kitchens.length)
    println("Area factor " + Constants.AREA_FACTOR)

    val nakedWorld = World.buildWorldGeometry(kitchens, giniParcels)
    val initialState = SimulationState(nakedWorld, kitchens, History.initialize(simulationLength, kitchens), 1)

    val finalState = evolve(initialState, populationGrowth, simulationLength + 1, soilQualityBasis)

    History.printParcels(finalState, hookParameters)
    History.printKitckens(finalState, hookParameters)

    finalState
  }


  def evolve(simulationState: SimulationState, populationGrowth: Double, simulationLenght: Int, soilQualityBasis: Double)(using MersenneTwister): SimulationState = {


    @tailrec
    def evolve0(simulationState: SimulationState): SimulationState = {
      if (simulationLenght - simulationState.year == 0 || simulationState.kitchens.size <= 1) simulationState
      else {
        val initialFood = simulationState.kitchens.map { k => Food(k.id, -Kitchen.foodNeeds(k)) }

        // Evolve rotation including loans
        val (afterRotationsSimulationState, foodAfterRotation) = Rotation.evolve(simulationState, soilQualityBasis, initialFood)

        // Compute soil quality and available nitrogen for each parcel before the rotation process until fertilities of the current year is computed
        implicit val fertilityMetricsByParcelAfterRotation: Fertility.AgronomicMetricsByParcel = afterRotationsSimulationState.world.parcels.map { p =>
          p.id -> Fertility.agronomicMetrics(p, soilQualityBasis)
        }.toMap

        // Process food donations
        val afterDonationFoods = FoodDonation.assign(foodAfterRotation, simulationState)

        // Process kitchen dynamics (population, emmigrants, absorptions, splits)
        val resizedSimulationState = Kitchen.evolve(afterRotationsSimulationState, populationGrowth, afterDonationFoods)

        // Process Fertiliy
        val afterFertilizationState = Fertility.assign(resizedSimulationState, soilQualityBasis)

        val finalHistory = afterFertilizationState.history.updateFoodBalances(afterFertilizationState.year, afterDonationFoods)
        val finalState = afterFertilizationState.copy(world = Loan.reset(afterFertilizationState.world), year = afterFertilizationState.year + 1, history = finalHistory)

        evolve0(finalState)
      }
    }

    evolve0(simulationState)
  }
}
