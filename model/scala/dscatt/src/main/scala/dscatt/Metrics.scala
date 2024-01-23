package dscatt

import Simulation.SimulationState
import Kitchen.*
import utils.*
import org.apache.commons.math3.stat.regression.SimpleRegression

implicit class HistoryDecorator(simulationState: SimulationState):

  def populationDynamic =
    simulationState.population.map(
      _.values.map(_.size).sum
    ).toArray

  def migrantsDynamic =
    simulationState.population.map(
      _.values.map(_.emigrants).sum
    ).toArray

  def averageKitchenSizeDynamic =
    simulationState.population.map(p =>
      average(p.values.map(_.size.toDouble).toSeq)
    ).toArray

  def numberOfKitchens = simulationState.population.map(_.keys.size).toArray

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

  def averageMulchingDynamic =
    simulationState.fertilityHistory.map(fh =>
      fh.map(_.mulchingMass)
    ).transpose.map(average(_)).toArray

  def totalMulching = simulationState.fertilityHistory.flatMap(fh =>
    fh.map(_.mulchingMass)
  ).sum

  def foodFromLoanOnFoodNeedsDynamic =
    simulationState.foodStats.map(fs =>
      average(fs.map { f =>
        val food = f._2
        food.fromLoan / food.needs * -1
      }.toSeq)
    ).toArray

  def foodFromDonationOnFoodNeedsDynamic =
    simulationState.foodStats.map(fs =>
      average(fs.map { f =>
        val food = f._2
        food.fromDonation / food.needs * -1
      }.toSeq)
    ).toArray

  def averageMilYieldDynamic =
    simulationState.foodStats.map { fs =>
      average(fs.flatMap { f =>
        val food = f._2
        if(food.fromMil != 0.0)
          Some(food.fromMil / food.milInCultureArea)
        else None
      }.toSeq)
    }.toArray

  // Divide by peanutSeedToFood to get seed (previously peanutFoodEquivalence)
  def averagePeanutYieldDynamic =
    simulationState.foodStats.map { fs =>
      average(fs.flatMap { f =>
        val food = f._2
        if (food.fromPeanut != 0.0)
          Some(food.fromPeanut / Constants.PEANUT_FOOD_EQUIVALENCE / food.peanutInCultureArea)
        else None
      }.toSeq)
    }.toArray

  // (culture + loan + donation) / needs
  def foodStress =
    simulationState.foodStats.map { fs =>
      -fs.map(_._2.fullProduction).sum / fs.map(_._2.needs).sum
    }.toArray


  def populationRSquareAndSlope: (Double, Double) =
    val regression = new SimpleRegression(true)
    populationDynamic.zipWithIndex foreach: (p,id)=>
      regression.addData(id,p)
    (regression.getRSquare, regression.getSlope)