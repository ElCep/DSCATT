package dscatt

import utils.CSVWrapper
import better.files.*
import File.*
import Kitchen.*
import Loan.*
import Diohine.*
import Simulation.*
import java.io.File as JFile

object History {

  case class PopulationStat(size: Int, births: Int, emigrants: Int, absorbedKitchens: Seq[KitchenID], splittedInto: Option[KitchenID])

  case class ParcelStat(size: Int, ownedArea: Double, loaned: Int, loanedArea: Double) {
    override def toString = s"($size, ${ownedArea.toInt}, ${loaned}, ${loanedArea.toInt})"
  }

  type PopulationStats = Map[KitchenID, PopulationStat]
  type ParcelStatsByKitchen = Map[KitchenID, ParcelStat]
  type Loans = Seq[Loan]
  type FoodStats = Map[KitchenID, Food]
  type History = Map[Int, YearHistory]
  type Fertilities = Map[KitchenID, Fertility.Metrics]
  type Herds = Map[KitchenID, Int]

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

    //    private def toSorted(fb: Seq[Food]) = {
    //      fb.sortBy(_.kitchenID).map(_.quantity)
    //    }

    def updateFoodBalances(year: Int, foods: Seq[Food]): History = {
      val historyOfYear = history(year)
      history.updated(year, historyOfYear.copy(foodStats = foods.map(f => f.kitchenID -> f).toMap))
    }

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
        val manureMassMean = parcelFertilities.map(_.manureMass).sum / parcelFertilities.size
        val mulchingMassMean = parcelFertilities.map(_.mulchingMass).sum / parcelFertilities.size
        val soilQualityMean = parcelFertilities.map(_.agronomicMetrics.soilQuality).sum / parcelFertilities.size
        val availableNitrogenMean = parcelFertilities.map(_.agronomicMetrics.availableNitrogen).sum / parcelFertilities.size

        k.id -> Fertility.Metrics(year, Croping.NotAssigned, manureMassMean, mulchingMassMean, Fertility.AgronomicMetrics(availableNitrogenMean, soilQualityMean))
      }.toMap

      history.updated(year, historyOfYear.copy(fertilities = historyOfYear.fertilities ++ newFertilities))
    }

    def updateHerdStats(year: Int, world: World, herdMap: Map[KitchenID, Int]) = {
      val historyOfTheYear = history(year)
      history.updated(year, historyOfTheYear.copy(herds = historyOfTheYear.herds ++ herdMap))
    }
  }

  protected case class YearHistory(
                                    year: Int,
                                    population: PopulationStats = Map(),
                                    parcelStats: ParcelStatsByKitchen = Map(),
                                    loans: Loans = Seq(),
                                    foodStats: FoodStats = Map(),
                                    fertilities: Fertilities = Map(),
                                    herds: Herds = Map()
                                  )

  def toParcelStats(yearLoans: Seq[Loan], parcels: Seq[Parcel]): ParcelStatsByKitchen = {

    val owned = parcels.groupBy(_.ownerID).map {
      case (kid, ps) => kid -> (ps.size, ps.map(_.area).sum)
    }
    val loaned = yearLoans.groupBy(_.to).map { case (kid, l) =>
      kid -> (l.size, l.map(_.parcel.area).sum)
    }

    owned.map { case (kid, (size, area)) =>
      val (loanedSize, loanedArea) = loaned.getOrElse(kid, (0, 0.0))
      kid -> ParcelStat(size, area, loanedSize, loanedArea)
    }
  }

  val doubleFormat = "%.2f"
  val locale = new java.util.Locale("en", "EN")

  def toDouble(s: Double) = doubleFormat.formatLocal(locale, s)

  def historyByYear(simulationState: SimulationState) = simulationState.history.keys.toSeq.sorted.map{simulationState.history(_)}

  // def sortedPopulation(simulationState: SimulationState) =


  def printKitckens(state: SimulationState, hookParameters: HookParameters) = {
    val header = Seq("Year", "KID", "Owd pcl", "Owd area", "Lnd pcl", "Lnd area", "Herd", "Manure", "Mulch", "N", "SQ", "FN", "FFC", "FFL", "FFD", "Balance", "FinX", "Size", "Births", "Migs", "Absor", "Split")

    val years = historyByYear(state).map { yearHistory =>
      val sortedPop = yearHistory.population.map { kp =>
        (kp._1, kp._2.size, kp._2.births, kp._2.emigrants, kp._2.absorbedKitchens, kp._2.splittedInto
        )
      }.toSeq.sortBy(_._1)

      val pStats = yearHistory.parcelStats
      val fbStats = yearHistory.foodStats
      val fertilityStats = yearHistory.fertilities

      sortedPop.map { p =>
        val fbStatsK = fbStats.getOrElse(p._1, Food(p._1))
        val fertilityStatK = fertilityStats.getOrElse(p._1, Fertility.Metrics(state.year))
        Seq(
          yearHistory.year,
          p._1.toString,
          s"${pStats(p._1).size}",
          s"${toDouble(pStats(p._1).ownedArea)}",
          s"${pStats(p._1).loaned}",
          s"${toDouble(pStats(p._1).loanedArea)}",
          s"${yearHistory.herds(p._1)}",
          s"${fertilityStatK.manureMass.toInt}",
          s"${fertilityStatK.mulchingMass.toInt}",
          s"${toDouble(fertilityStatK.agronomicMetrics.availableNitrogen)}",
          s"${toDouble(fertilityStatK.agronomicMetrics.soilQuality)}",
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

  def printParcels(state: SimulationState, hookParameters: HookParameters) = {
    val first20 = state.world.parcels.take(20)


    val header = Seq("Year", "ID", "Area", "QS", "N/ha", "Manure/ha", "Mulch/ha", "#Faid","Yield/ha", "for crop")
    val years = state.history.keys.toSeq.sorted.map { y =>
      first20.map { p =>
        val fertility = p.fertilityHistory(y - 1)
        //val id = p.tinyID
        val area = p.area

        Seq(
          y,
          p.id,
          toDouble(area),
          toDouble(fertility.agronomicMetrics.soilQuality),
          toDouble(fertility.agronomicMetrics.availableNitrogen / area),
          toDouble(fertility.manureMass / area),
          toDouble(fertility.mulchingMass / area),
          toDouble(p.faidherbiaTrees),
          toDouble(Kitchen.parcelFoodProduction(fertility.crop, area, fertility.agronomicMetrics) / area),
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