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

    val nakedWorld = World.buildWorldGeometry(kitchens.length, giniParcels, giniTolerance, maximumNumberOfParcels, seed, parcelOutputPath)
    val initialState = SimulationState(nakedWorld, kitchens, History.initialize(simulationLength, kitchens), 1)

    val finalState = evolve(initialState, populationGrowth, simulationLength + 1)

    History.print(finalState, true)
  }


  def evolve(simulationState: SimulationState, populationGrowth: Double, simulationLenght: Int)(using MersenneTwister): SimulationState = {

    @tailrec
    def evolve0(simulationState: SimulationState): SimulationState = {
      if (simulationLenght - simulationState.year == 0) simulationState
      else {
        val afterRotationsSimulationState = Rotation.evolve(simulationState)
        val foodAssessment = afterRotationsSimulationState.kitchens.map { k => Kitchen.foodBalance(afterRotationsSimulationState.world, k) }

        //FIXME: Compute food exchange
        // val afterExchangeFoodAssessment = FoodExchange.evolve(afterRotationsSimulationState, foodAssessment)

        val resizedSimulationState = Kitchen.evolve(afterRotationsSimulationState, populationGrowth, foodAssessment)

        // Problème: si des kitchens split, qu'on-t-elle a bouffer cette année ? Est-ce qu'on les met dans le pool pour l'année d'après ??

        //        println(" ---- EVOLVED ---- " + resizedSimulationState.world.parcels.length)
        //        println("FALLOW " + World.fallowParcels(resizedSimulationState.world).length)
        //        println("PEANUT " + World.peanutParcels(resizedSimulationState.world).length)
        //        println("MIL " + World.milParcels(resizedSimulationState.world).length)
        //        println("SUM " + (World.fallowParcels(resizedSimulationState.world).length + World.milParcels(resizedSimulationState.world).length + World.peanutParcels(resizedSimulationState.world).length))
        //        println("NOT ASSIGNED " + resizedSimulationState.world.parcels.filter { p => p.crop == NotAssigned }.length)
        //
        //        println("\nZONE 1 : " + World.zoneOneParcels(resizedSimulationState.world).length)
        //        println("ZONE 2 : " + World.zoneTwoParcels(resizedSimulationState.world).length)
        //        println("ZONE 3: " + World.zoneThreeParcels(resizedSimulationState.world).length)
        evolve0(resizedSimulationState.copy(world = Loan.reset(resizedSimulationState.world), year = resizedSimulationState.year + 1))
      }
    }

    val finalState = evolve0(simulationState)

    finalState
  }
}
