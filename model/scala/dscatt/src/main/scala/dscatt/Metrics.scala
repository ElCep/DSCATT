package dscatt

import Simulation.SimulationState
import Kitchen.*


package object dscatt

implicit class HistoryDecorator(simulationState: SimulationState):

  private def population = History.historyByYear(simulationState).map {
    _.population
  }

  def populationDynamic = population.map(_.values.map(_.size).sum).toArray

  def herdDynamic =
    History.historyByYear(simulationState).map {
      _.herds
    }.map {
      _.values.sum
    }.toArray

  def averageNitrogenDynamic =
    simulationState.world.parcels.map(p =>
      p.fertilityHistory.map(_.agronomicMetrics.availableNitrogen)
    ).transpose.map(nitrogenPerYear =>
      nitrogenPerYear.sum / nitrogenPerYear.size
    ).toArray

  def averageSoilQualityDynamic =
    simulationState.world.parcels.map(p =>
      p.fertilityHistory.map(_.agronomicMetrics.soilQuality)
    ).transpose.map(soilQualityPerYear =>
      soilQualityPerYear.sum / soilQualityPerYear.size
    ).toArray

  def averageInexcessDynamic =
    History.historyByYear(simulationState).map { hY =>
      val inexcess = hY.foodStats.map(_._2.inexess)
      inexcess.sum / inexcess.size
    }.toArray

  def numberOfUnbalancedKitchen =
    val balances = History.historyByYear(simulationState).flatMap { hY =>
      hY.foodStats.map(_._2.toBalance)
    }
    balances.filter(_ < 0).size