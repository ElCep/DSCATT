package dscatt

import utils.CSVWrapper
import better.files.*
import File.*
import Kitchen.*
import Loan.*
import Diohine.*
import Simulation.*

import java.io.File as JFile
import Data.*
import dscatt.Fertility.SoilQuality

object History {

  case class PopulationStat(size: Int, births: Int, emigrants: Int, absorbedKitchens: Seq[KitchenID], splittedInto: Option[KitchenID])

  case class ParcelStat(farmedParcelsQuantity: Int, farmedArea: Double, loanedParcelsQuantity: Int, loanedArea: Double) {
    override def toString = s"($farmedParcelsQuantity, ${farmedArea.toInt}, ${loanedParcelsQuantity}, ${loanedArea.toInt})"
  }

  type PopulationStats = Map[KitchenID, PopulationStat]
  type ParcelStatsByKitchen = Map[KitchenID, ParcelStat]
  type Loans = Seq[Loan]
  type FoodStats = Map[KitchenID, Food]
  type History = Map[Int, YearHistory]
  type Fertilities = Map[KitchenID, Fertility.Metrics]
  type Herds = Map[KitchenID, Int]
  type KitchenProfile = Map[KitchenID, KitchenProfileID]

  def initialize(simulationLenght: Int, kitchens: Seq[Kitchen]): History = {
    (1 to simulationLenght).map { y =>
      y -> YearHistory(year = y)
    }.toMap
  }

  implicit class HistoryWrap(history: History) {
    def updateLoans(year: Int, loans: Loans, parcels: Seq[Parcel]) = {
      val historyOfYear = history(year)
      val parcelStats = History.toParcelStats(loans, parcels)
      history.updated(year, historyOfYear.copy(loans = loans, parcelStats = parcelStats))
    }

    def updatePopulations(year: Int, populations: Seq[(KitchenID, PopulationStat)]): History = {
      val historyOfYear = history(year)
      history.updated(year, historyOfYear.copy(population = populations.toMap))
    }

    def updateKitchenProfile(year: Int, kitchens: Seq[Kitchen]): History =
      val historyOfYear = history(year)
      history.updated(year, historyOfYear.copy(kitchenProfile = kitchens.map(k=> k.id-> k.profileID).toMap))

    //    private def toSorted(fb: Seq[Food]) = {
    //      fb.sortBy(_.kitchenID).map(_.quantity)
    //    }

    def updateFoods(year: Int, foods: Seq[Food]): History = {
      val historyOfYear = history(year)
      history.updated(year, historyOfYear.copy(foodStats = foods.map(f => f.kitchenID -> f).toMap))
    }
    
    def updateEffectiveFallowRatio(year: Int, effectiveFallowProportion: Double) =
      val historyOfYear = history(year)
      history.updated(year, historyOfYear.copy(effectiveFallowRatio = effectiveFallowProportion))
      

    def updateParcelStatsAfterPopulationEvolution(year: Int, allKitchens: Seq[Kitchen], world: World) = {
      val historyOfYear = history(year)
      val newKitchens = allKitchens.filterNot(i => historyOfYear.parcelStats.keys.toSeq.contains(i.id))
      val newParcelStats = newKitchens.map { k =>
        val owned = World.ownedParcelsForKitchen(world, k)
        k.id -> ParcelStat(owned.size, owned.map(_.area).sum, 0, 0.0)
      }.toMap
      history.updated(year, historyOfYear.copy(parcelStats = historyOfYear.parcelStats ++ newParcelStats))
    }

    // Store average fertility metrics by kitchen
    def updateFertilitySats(year: Int, world: World, kitchens: Seq[Kitchen]) = {
      val historyOfYear = history(year)
      val newFertilities = kitchens.map { k =>
        val parcelFertilities = World.farmedParcelsForKitchen(world, k).flatMap {
          _.fertilityHistory.filter(md => md.year == year)
        }
        val manureMassMean = parcelFertilities.map(_.manureMassByHa).sum / parcelFertilities.size
        val mulchingMassMean = parcelFertilities.map(_.mulchingMassByHa).sum / parcelFertilities.size
        val annualSoilQualityMean = parcelFertilities.map(_.agronomicMetrics.soilQuality.annualSoilQuality).sum / parcelFertilities.size
        val residualSoilQualityMean = parcelFertilities.map(_.agronomicMetrics.soilQuality.residualSoilQuality).sum / parcelFertilities.size
        val availableNitrogenMean = parcelFertilities.map(_.agronomicMetrics.availableNitrogen).sum / parcelFertilities.size

        k.id -> Fertility.Metrics(year, Croping.Fallow, manureMassMean, mulchingMassMean, Fertility.AgronomicMetrics(availableNitrogenMean, SoilQuality(residualSoilQualityMean, annualSoilQualityMean)))
      }.toMap

      history.updated(year, historyOfYear.copy(fertilities = historyOfYear.fertilities ++ newFertilities))
    }

    def updateHerdStats(year: Int, herdMap: Map[KitchenID, Int]) = {
      val historyOfTheYear = history(year)
      history.updated(year, historyOfTheYear.copy(herds = historyOfTheYear.herds ++ herdMap))
    }
  }

  protected case class YearHistory(
                                    year: Int,
                                    population: PopulationStats = Map(),
                                    kitchenProfile: KitchenProfile = Map(),
                                    parcelStats: ParcelStatsByKitchen = Map(),
                                    loans: Loans = Seq(),
                                    foodStats: FoodStats = Map(),
                                    fertilities: Fertilities = Map(),
                                    herds: Herds = Map(),
                                    effectiveFallowRatio: Double = 0.0
                                  )

  def toParcelStats(yearLoans: Seq[Loan], parcels: Seq[Parcel]): ParcelStatsByKitchen = {

    val cultivated = parcels.groupBy(_.farmerID).map {
      case (kid, ps) =>
        val pInCulture = World.parcelsInCultureForKitchenID(ps, kid)
        kid -> (pInCulture.size, pInCulture.map(_.area).sum)
    }
    val loaned = yearLoans.groupBy(_.to).map { case (kid, l) =>
      kid -> (l.size, l.map(_.parcel.area).sum)
    }

    cultivated.map { case (kid, (cultivatedSize, cultivatedArea)) =>
      val (loanedSize, loanedArea) = loaned.getOrElse(kid, (0, 0.0))
      kid -> ParcelStat(cultivatedSize, cultivatedArea, loanedSize, loanedArea)
    }
  }

  val doubleFormat = "%.2f"
  val locale = new java.util.Locale("en", "EN")

  def toDouble(s: Double) = doubleFormat.formatLocal(locale, s)

  def historyByYear(simulationState: SimulationState) = simulationState.history.keys.toSeq.sorted.map{simulationState.history(_)}

  // def sortedPopulation(simulationState: SimulationState) =


  def printKitckens(state: SimulationState, hookParameters: HookParameters) = {
    val header = Seq("Year", "KID", "ProfID","Owd pcl", "Owd area", "Lnd pcl", "Lnd area", "Herd", "Manure", "Mulch", "N", "YSQ", "FN", "FFC", "FFL", "FFD", "Balance", "FinX", "Size", "Births", "Migs", "Absor", "Split")

    val years = historyByYear(state).map { yearHistory =>
      val sortedPop = yearHistory.population.map { kp =>
        (kp._1, kp._2.size, kp._2.births, kp._2.emigrants, kp._2.absorbedKitchens, kp._2.splittedInto
        )
      }.toSeq.sortBy(_._1)

      val pStats = yearHistory.parcelStats
      val fbStats = yearHistory.foodStats
      val fertilityStats = yearHistory.fertilities
      val kitchenProfileStats = yearHistory.kitchenProfile

      sortedPop.map { p =>
        val fbStatsK = fbStats.getOrElse(p._1, Food(p._1))
        val fertilityStatK = fertilityStats.getOrElse(p._1, Fertility.Metrics(state.year))

        Seq(
          yearHistory.year,
          p._1.toString,
          s"${kitchenProfileStats(p._1)}",
          s"${pStats(p._1).farmedParcelsQuantity}",
          s"${toDouble(pStats(p._1).farmedArea)}",
          s"${pStats(p._1).loanedParcelsQuantity}",
          s"${toDouble(pStats(p._1).loanedArea)}",
          s"${yearHistory.herds(p._1)}",
          s"${fertilityStatK.manureMassByHa.toInt}",
          s"${fertilityStatK.mulchingMassByHa.toInt}",
          s"${toDouble(fertilityStatK.agronomicMetrics.availableNitrogen)}",
          s"${toDouble(fertilityStatK.agronomicMetrics.soilQuality.annualSoilQuality)}",
          s"${fbStatsK.needs.toInt}",
          s"${fbStatsK.fromCulture.toInt}",
          s"${fbStatsK.fromLoan.toInt}",
          s"${fbStatsK.fromDonation.toInt}",
          s"${fbStatsK.toBalance.toInt}",
          s"${fbStatsK.inexess.toInt}",
          p._2.toString,
          p._3.toString,
          p._4.toString,
          p._5.mkString(","),
          p._6.map(_.toString).getOrElse("")
        )
      }
    }

    val pops = state.history.values.map { yh => yh.year -> yh.population.values.map {
      _.size
    }.sum
    }.toMap

    hookParameters.displayKitchens match {
      case true =>
        years.zipWithIndex.foreach { (table, ind) =>
          val tableWithHeader = header +: table
          println(Tabulator.formatTable(tableWithHeader))
        }
      case _ =>
    }

    hookParameters.hookFile match {
      case Some(hf: HookFile) =>
        hf.kitchens match
          case true =>
            val content = (header +: years.flatten).toCSV()
            val file = File(hf.outputPath + "/kitchens.csv")
            file.overwrite(content)
          case false =>
      case None =>
    }

  }

  def printParcels(state: SimulationState, hookParameters: HookParameters, data: Data) = {
    val first20 = state.world.parcels.take(20)


    val header = Seq("Year", "ID", "Area", "YQS", "N/ha", "Manure/ha", "Mulch/ha", "#Faid","Yield/ha", "for crop")
    val years = state.history.keys.toSeq.sorted.map { y =>
      first20.map { p =>
        val fertility = p.fertilityHistory(y - 1)
        //val id = p.tinyID
        val area = p.area

        Seq(
          y,
          p.id,
          toDouble(area),
          toDouble(fertility.agronomicMetrics.soilQuality.annualSoilQuality),
          toDouble(fertility.agronomicMetrics.availableNitrogen / area),
          toDouble(fertility.manureMassByHa / area),
          toDouble(fertility.mulchingMassByHa / area),
          toDouble(p.faidherbiaTreesByHa),
          toDouble(Kitchen.parcelFoodProduction(p, data, state.year) / area),
          fertility.crop.display
        )
      }
    }

    hookParameters.displayParcels match {
      case true =>
        years.foreach { table =>
          val tableWithHeader = header +: table
          println(Tabulator.formatTable(tableWithHeader))
        }
      case _ =>
    }

    hookParameters.hookFile match {
      case Some(hf: HookFile) =>
        hf.parcels match {
          case true =>
            val content = (header +: years.flatten).toCSV()
            val file = File(s"${hf.outputPath}/parcels.csv")
            file.overwrite(content)
          case false =>
        }
      case None =>
    }

  }
}