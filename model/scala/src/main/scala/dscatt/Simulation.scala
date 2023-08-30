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
             populationGrowth: Double,
             kitchenPartition: KitchenPartition = KitchenPartition((KitchenProfile.default, 1)),
             supportPolicy: SupportPolicy,
             simulationLength: Int = 20,
             soilQualityBasis: Double = 1.0,
             parcelOutputPath: Option[String] = None
           ) = {

    given MersenneTwister(seed)

    val kitchens = Kitchen.buildKitchens(kitchenPartition)

    println("NB KITCH " + kitchens.length)
    println("Area factor " + Constants.AREA_FACTOR)

    val nakedWorld = World.buildWorldGeometry(kitchens, giniParcels, giniTolerance, maximumNumberOfParcels, seed, parcelOutputPath)
    val initialState = SimulationState(nakedWorld, kitchens, History.initialize(simulationLength, kitchens), 1)

    val finalState = evolve(initialState, populationGrowth, simulationLength + 1, soilQualityBasis)

    History.printKitckens(finalState, true)
    //History.printParcels(finalState)
  }


  def evolve(simulationState: SimulationState, populationGrowth: Double, simulationLenght: Int, soilQualityBasis: Double)(using MersenneTwister): SimulationState = {


    @tailrec
    def evolve0(simulationState: SimulationState): SimulationState = {
      if (simulationLenght - simulationState.year == 0 || simulationState.kitchens.size <= 1) simulationState
      else {
        val initialFoodNeeds = simulationState.kitchens.map { k => k.id -> -Kitchen.foodNeeds(k) }

        // Compute soil quality and available nitrogen for each parcel before the rotation process until fertilities of the current year is computed
        implicit val fertilityMetricsByParcel: Fertility.AgronomicMetricsByParcel = simulationState.world.parcels.map { p =>
          p.id -> Fertility.agronomicMetrics(p, soilQualityBasis)
        }.toMap

        // Evolve rotation including loans
        val (afterRotationsSimulationState, autonomousFoodBalance) = Rotation.evolve(simulationState)
        val afterLoanFoodBalance = afterRotationsSimulationState.kitchens.map { k => Kitchen.foodBalance(afterRotationsSimulationState.world, k) }

        // Process food donations
        val afterDonationFoodBalance = FoodDonation.assign(afterLoanFoodBalance)

        // Process kitchen dynamics (population, emmigrants, absorptions, splits)
        val resizedSimulationState = Kitchen.evolve(afterRotationsSimulationState, populationGrowth, afterDonationFoodBalance)

        // Process Fertiliy
        val afterFertilizationState = Fertility.assign(resizedSimulationState, soilQualityBasis)

        val finalHistory = afterFertilizationState.history.updateFoodBalances(afterFertilizationState.year, initialFoodNeeds, autonomousFoodBalance, afterLoanFoodBalance, afterDonationFoodBalance)
        val finalState = afterFertilizationState.copy(world = Loan.reset(afterFertilizationState.world), year = afterFertilizationState.year + 1, history = finalHistory)
        evolve0(finalState)
      }
    }

    evolve0(simulationState)
  }
}
