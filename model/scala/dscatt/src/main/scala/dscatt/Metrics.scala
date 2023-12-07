package dscatt

import Simulation.SimulationState
import Kitchen.*
import utils.*

implicit class HistoryDecorator(simulationState: SimulationState):

  def populationDynamic =
    simulationState.population.map(
      _.values.map(_.size).sum
    ).toArray

  def migrantsDynamic =
    simulationState.population.map(
      _.values.map(_.emigrants).sum
    ).toArray

  def herdDynamic = simulationState.herds.map(_.values.sum).toArray

  def averageNitrogenDynamic =
    simulationState.fertilityHistory.map(fh =>
      fh.map(_.agronomicMetrics.availableNitrogen)
    ).transpose.map(average(_)).toArray

  def averageSoilQualityDynamic =
    simulationState.fertilityHistory.map(fh =>
      fh.map(_.agronomicMetrics.soilQuality)
    ).transpose.map(average(_)).toArray

  def averageInexcessDynamic: Array[Double] =
    simulationState.foodStats.map(x =>
      average(x.map(_._2.inexess).toArray)
    ).toArray

  def numberOfUnbalancedKitchen =
    simulationState.foodStats.flatMap(
      _.map(_._2.toBalance).filter(_ < 0)
    ).size

  def loanedAreaDynamic = simulationState.parcelStats.flatMap(_.map(_._2.loanedArea))

  def totalLoanedArea = loanedAreaDynamic.sum

  def averageManureDynamic =
    simulationState.fertilityHistory.map(fh =>
      fh.map(_.manureMass)
    ).transpose.map(average(_)).toArray

  def totalManure = simulationState.fertilityHistory.flatMap(fh =>
      fh.map(_.manureMass)
    ).sum

  def foodFromLoanOnFoodNeedsDynamic =
    simulationState.foodStats.map(fs=>
      average(fs.map { f=>
        val food = f._2
        food.fromLoan / food.needs * -1
      }.toSeq)
    ).toArray

  def foodFromDonationOnFoodNeedsDynamic =
    simulationState.foodStats.map(fs=>
      average(fs.map { f=>
        val food = f._2
        food.fromDonation / food.needs * -1
      }.toSeq)
    ).toArray