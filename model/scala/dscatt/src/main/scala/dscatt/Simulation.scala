package dscatt

import Croping.*
import Diohine.{HookFile, HookParameters}
import History.History
import Kitchen.Food
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
             kitchenMinimumSize: Int = 4, // exposed for calibration
             kitchenMaximumSize: Int = 24, // exposed for calibration
             splitKitchenOffspringSize: Int = 6, // exposed for calibration
             peanutSeedToFood: Double, // exposed for calibration
             hookParameters: HookParameters
           ) = {

    given MersenneTwister(seed)

    val kitchens = Kitchen.buildKitchens(kitchenPartition)

    println("NB KITCH " + kitchens.length)
    println("Area factor " + Constants.AREA_FACTOR)

    val nakedWorld = World.buildWorldGeometry(kitchens, giniParcels)

    println("area totale " + nakedWorld.parcels.map(_.area).sum)
    val initialHistory = History.initialize(simulationLength, kitchens)
    val initialState = SimulationState(nakedWorld, kitchens, initialHistory, 1)

    // Two years warming up
    val warmedUpState = evolve(initialState, 0.0, 3, soilQualityBasis, false, 0, 0, 0, 0.0).copy(history = initialHistory, year = 1)

    val finalState = evolve(warmedUpState, populationGrowth, simulationLength + 1, soilQualityBasis, true, kitchenMinimumSize, kitchenMaximumSize, splitKitchenOffspringSize, peanutSeedToFood)

    History.printParcels(finalState, hookParameters)
    History.printKitckens(finalState, hookParameters)

    finalState
  }


  def evolve(
              simulationState: SimulationState,
              populationGrowth: Double,
              simulationLenght: Int,
              soilQualityBasis: Double,
              emigrationProcess: Boolean,
              kitchenMinimumSize: Int,
              kitchenMaximumSize: Int,
              splitKitchenOffringSize: Int,
              peanutSeedToFood: Double
            )(using MersenneTwister): SimulationState = {
    
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
        val resizedSimulationState = Kitchen.evolve(
          afterRotationsSimulationState,
          populationGrowth,
          afterDonationFoods,
          emigrationProcess
        )

        // Process Fertiliy
        val afterFertilizationState = Fertility.assign(resizedSimulationState, soilQualityBasis)

        val finalHistory = afterFertilizationState.history.updateFoods(afterFertilizationState.year, afterDonationFoods)

        val tot = finalHistory.get(afterRotationsSimulationState.year).map {
          _.foodStats.values.map(x => x.fromCulture + x.fromLoan).sum
        }.getOrElse(0.0)

        val loan = finalHistory.get(afterRotationsSimulationState.year).map {
          _.foodStats.values.map(x => x.fromLoan).sum
        }.getOrElse(0.0)

        val mil = Kitchen.parcelsFoodProduction(World.milParcels(afterRotationsSimulationState.world.parcels))
        val peanut = Kitchen.parcelsFoodProduction(World.peanutParcels(afterRotationsSimulationState.world.parcels))

        println("TOTAL MIL " + afterRotationsSimulationState.year + " : " + mil + " :: " + peanut + " :: " + tot + " :: " + mil / tot + " :: " + loan / tot)


        val finalState = afterFertilizationState.copy(world = Loan.reset(afterFertilizationState.world), year = afterFertilizationState.year + 1, history = finalHistory)
        println("not mil, peanut, fallow " + finalState.world.parcels.filter(x => x.crop == Mil).size + ", "
          + finalState.world.parcels.filter(x => x.crop == Peanut).size + ", "
          + finalState.world.parcels.filter(x => x.crop == Fallow).size)

        evolve0(finalState)
      }
    }

    // FIXME: get rid off the var once calibration is done
    Constants.PEANUT_FOOD_EQUIVALENCE = peanutSeedToFood
    Constants.KITCHEN_MINIMUM_SIZE = kitchenMinimumSize
    Constants.KITCHEN_MAXIMUM_SIZE = kitchenMaximumSize
    Constants.SPLIT_KITCHEN_OFFSPRING_SIZE = splitKitchenOffringSize
    evolve0(simulationState)
  }
}
