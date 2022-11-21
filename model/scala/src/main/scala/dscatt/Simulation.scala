package dscatt

import dscatt.Croping.*
import dscatt.History.History
import org.apache.commons.math3.random.MersenneTwister

import scala.annotation.tailrec

object Simulation {

  case class SimulationState(world: World, kitchens: Seq[Kitchen], history: History, year: Int)

  def apply(
             seed: Long,
             giniParcels: Double,
             giniTolerance: Double = 0.01,
             maximumNumberOfParcels: Int = 200,
             herdSize: Int = 100,
             giniHerd: Double,
             populationGrowth: Double,
             kitchenPartition: KitchenPartition = KitchenPartition((KitchenProfile.default, 1)),
             simulationLength: Int = 20,
             parcelOutputPath: Option[String] = None
           ) = {

    given MersenneTwister(seed)

    val kitchens = Kitchen.buildKitchens(kitchenPartition)

    println("NB KITCH " + kitchens.length)
    println("Area factor " + Constants.AREA_FACTOR)

    val nakedWorld = World.buildWorldGeometry(kitchens, giniParcels, giniTolerance, maximumNumberOfParcels, seed, parcelOutputPath)
    val initialState = SimulationState(nakedWorld, kitchens, History.initialize(simulationLength, kitchens), 1)

    val finalState = evolve(initialState, populationGrowth, simulationLength + 1)

    History.print(finalState, true)
  }


  def evolve(simulationState: SimulationState, populationGrowth: Double, simulationLenght: Int)(using MersenneTwister): SimulationState = {

    @tailrec
    def evolve0(simulationState: SimulationState): SimulationState = {
      if (simulationLenght - simulationState.year == 0 || simulationState.kitchens.size <= 0) simulationState
      else {
        val initialFoodNeeds = simulationState.currentFoodNeeds

        // Evolve rotation including loans
        val (afterRotationsSimulationState, autonomousFoodBalance) = Rotation.evolve(simulationState)
        val afterLoanFoodBalance = afterRotationsSimulationState.currentFoodBalances

        // Process food donations
        val afterDonationFoodBalance = FoodDonation.assign(afterLoanFoodBalance)

        // Process kitchen dynamics (population, emmigrants, absorptions, splits)
        val resizedSimulationState = Kitchen.evolve(afterRotationsSimulationState, populationGrowth, afterDonationFoodBalance)

        val finalHistory = resizedSimulationState.history.updateFoodBalances(resizedSimulationState.year, initialFoodNeeds, autonomousFoodBalance, afterLoanFoodBalance, afterDonationFoodBalance)
        val finalState = resizedSimulationState.copy(world = Loan.reset(resizedSimulationState.world), year = resizedSimulationState.year + 1, history = finalHistory)
        evolve0(finalState)
      }
    }

    evolve0(simulationState)
  }

  implicit class AState(simulationState: SimulationState) {
    def currentFoodBalances = simulationState.kitchens.map { k => Kitchen.foodBalance(simulationState.world, k) }
    def currentFoodNeeds = simulationState.kitchens.map { k => k.id-> - Kitchen.foodNeeds(k) }
  }
}
