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

  def averageAnnualSoilQualityDynamic =
    simulationState.fertilityHistory.map(fh =>
      fh.map(_.agronomicMetrics.soilQuality.annualSoilQuality)
    ).transpose.map(average(_)).toArray

  def averageResidualSoilQualityDynamic =
    simulationState.fertilityHistory.map(fh =>
      fh.map(_.agronomicMetrics.soilQuality.residualSoilQuality)
    ).transpose.map(average(_)).toArray

  def averageSQByNitrogenDynamic =
    simulationState.fertilityHistory.map(fh =>
      fh.map(x => x.agronomicMetrics.soilQuality.annualSoilQuality * x.agronomicMetrics.availableNitrogen)
    ).transpose.map(average(_)).toArray

  def averageInexcessDynamic: Array[Double] =
    simulationState.foodStats.map(x =>
      average(x.map(_._2.inexess).toArray)
    ).toArray

  def numberOfUnbalancedKitchen =
    simulationState.foodStats.flatMap(
      _.map(_._2.toBalance).filter(_ < 0)
    ).size

  def loanedAreaDynamic =
    simulationState.parcelStats.map(_.map(_._2.loanedArea).sum).toArray

  def totalLoanedArea = loanedAreaDynamic.sum

  def averageManureDynamic =
    simulationState.fertilityHistory.map(fh =>
      fh.map(_.manureMassByHa)
    ).transpose.map(average(_)).toArray

  def totalManure = simulationState.fertilityHistory.flatMap(fh =>
    fh.map(_.manureMassByHa)
  ).sum

  def averageMulchingDynamic =
    simulationState.fertilityHistory.map(fh =>
      fh.map(_.mulchingMassByHa)
    ).transpose.map(average(_)).toArray

  def totalMulching = simulationState.fertilityHistory.flatMap(fh =>
    fh.map(_.mulchingMassByHa)
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
      average(fs.map { f =>
        val food = f._2
        if (food.milInCultureArea != 0.0)
          food.fromMil / food.milInCultureArea
        else 0.0
      }.toSeq)
    }.toArray

  // Divide by peanutSeedToFood to get seed (previously peanutFoodEquivalence)
  def averagePeanutYieldDynamic(using data: Data) =
    simulationState.foodStats.map { fs =>
      average(fs.map { f =>
        val food = f._2
        if (food.peanutInCultureArea != 0.0)
          food.fromPeanut / data.PEANUT_FOOD_EQUIVALENCE / food.peanutInCultureArea
        else 0.0
      }.toSeq)
    }.toArray

  // (culture + loan + donation) / needs
  def foodStress =
    simulationState.foodStats.map { fs =>
      -fs.map(_._2.fullProduction).sum / fs.map(_._2.needs).sum
    }.toArray


  def populationRSquareAndSlope: (Double, Double) =
    val regression = new SimpleRegression(true)
    populationDynamic.zipWithIndex foreach : (p, id) =>
      regression.addData(id, p)
    (regression.getRSquare, regression.getSlope)

  def effectiveFallowRatioDynamic =
    (History.historyByYear(simulationState) map : h =>
      h.effectiveFallowRatio
      ).toArray

  def numberOfAbsorbedKitchens =
    simulationState.population.flatMap {
      _.map(_._2.absorbedKitchens.length)
    }.sum

  def kitchenProfileDynamic =
    simulationState.kitchenProfile.map:
      _.groupBy(_._2)
        .map(y => y._1 -> y._2.size)
    .toArray

  def kitchenProfileRatiosDynamic =
    kitchenProfileDynamic.map: p=>
      val nbKitchen = p.values.sum
      p.map(x=> x._1 -> x._2.toDouble / nbKitchen)
      
      
