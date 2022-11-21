package dscatt

import dscatt.FoodDonation.FoodDonation
import dscatt.Kitchen.{FoodBalance, KitchenID}
import dscatt.Loan.Loan
import dscatt.Simulation.SimulationState

object History {

  case class PopulationStat(size: Int, births: Int, emigrants: Int, absorbedKitchens: Seq[KitchenID], splittedInto: Option[KitchenID])

  case class ParcelStat(size: Int, ownedArea: Double, loaned: Int, loanedArea: Double) {
    override def toString = s"($size, ${ownedArea.toInt}, ${loaned}, ${loanedArea.toInt})"
  }

  case class FoodBalanceOverYear(initialFoodNeeds: Int = 0, autonomousFoodBalance: Int = 0, afterLoanFoodBalance: Int = 0, afterDonationFoodBalance: Int = 0)

  type PopulationStats = Map[KitchenID, PopulationStat]
  type ParcelStats = Map[KitchenID, ParcelStat]
  type Loans = Seq[Loan]
  type FoodBalanceStats = Map[KitchenID, FoodBalanceOverYear]
  type History = Map[Int, YearHistory]

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

    private def toSorted(fb: Seq[FoodBalance]) = {
      fb.sortBy(_.kitchen.id).map(_.balance)
    }

    def updateFoodBalances(year: Int, initialFoodNeeds: Seq[(KitchenID, Double)], autounomousFoodBalances: Seq[FoodBalance], afterLoanFoodBalances: Seq[FoodBalance], afterGiftFoodBalance: Seq[FoodBalance]): History = {
      val historyOfYear = history(year)
      val foodBalanceStats = initialFoodNeeds.sortBy(_._1).zip(toSorted(autounomousFoodBalances).zip(toSorted(afterLoanFoodBalances)).zip(toSorted(afterGiftFoodBalance))).map { case ((id, ifn), ((q1, q2), q3)) =>
        id -> FoodBalanceOverYear(ifn.toInt, q1.toInt, q2.toInt, q3.toInt)
      }.toMap
      history.updated(year, historyOfYear.copy(foodBalanceStats = foodBalanceStats))
    }

    def updateParcelStatsAfterPopulationEvolution(year: Int, allKitchenIDs: Seq[KitchenID]) = {
      val historyOfYear = history(year)
      val newIDs = allKitchenIDs diff historyOfYear.parcelStats.keys.toSeq
      val newParcelStats = newIDs.map(i => i -> ParcelStat(0, 0.0, 0, 0.0)).toMap
      history.updated(year, historyOfYear.copy(parcelStats = historyOfYear.parcelStats ++ newParcelStats))
    }
  }

  protected case class YearHistory(
                                    year: Int,
                                    fertility: Double = 0.0,
                                    population: PopulationStats = Map(),
                                    parcelStats: ParcelStats = Map(),
                                    loans: Loans = Seq(),
                                    foodBalanceStats: FoodBalanceStats = Map()
                                  )

  def toParcelStats(yearLoans: Seq[Loan], parcels: Seq[Parcel]): ParcelStats = {

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

  def print(state: SimulationState, verbose: Boolean = false) = {
    state.history.keys.toSeq.sorted.foreach { y =>
      val yearHistory = state.history(y)
      println(s"\nYEAR $y\n")
      val sortedPop = yearHistory.population.map { kp =>
        (kp._1, kp._2.size, kp._2.births, kp._2.emigrants, kp._2.absorbedKitchens, kp._2.splittedInto
        )
      }.toSeq.sortBy(_._1)
      val totalPop = sortedPop.map { x => Seq(x._2, x._3, x._4, x._5.size, x._6.map { _ => 1 }.getOrElse(0)) }.transpose.map {
        _.sum
      }
      val pStats = yearHistory.parcelStats
      val fbStats = yearHistory.foodBalanceStats

      val table = Seq(Seq("KID", "Owned Size/Area", "Loaned Size/Area", "Food Balance (initFN/AFB/ALFB/ADFB)", "Size", "Births", "Migs", "Absor", "Split")) ++ sortedPop.map(p =>
        val fbStatsK = fbStats.getOrElse(p._1, FoodBalanceOverYear())
          Seq(
          p._1.toString,
          s"${pStats(p._1).size}, ${pStats(p._1).ownedArea.toInt}",
          s"${pStats(p._1).loaned}, ${pStats(p._1).loanedArea.toInt}",
          s"${fbStatsK.initialFoodNeeds}, ${fbStatsK.autonomousFoodBalance}, ${fbStatsK.afterLoanFoodBalance}, ${fbStatsK.afterDonationFoodBalance}",
          p._2.toString,
          p._3.toString,
          p._4.toString,
          p._5.mkString(","),
          p._6.map(_.toString).getOrElse("")
        )
      )

      println(s"PARCELS ${pStats.map(_._2.size).sum} POPULATION ${totalPop(0)} / ${totalPop(1)} / ${totalPop(2)} KITCHENS ${totalPop(3)} / ${totalPop(4)}")
      if (verbose) println(Tabulator.formatTable(table))
    }
  }
}