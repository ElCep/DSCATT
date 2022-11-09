package dscatt

import dscatt.Kitchen.KitchenID
import dscatt.Loan.Loan
import dscatt.Simulation.SimulationState

object History {

  case class PopulationStat(size: Int, births: Int, emigrants: Int, absorbedKitchens: Seq[KitchenID], splittedInto: Option[KitchenID])

  case class ParcelStat(size: Int, ownedArea: Double, loaned: Int, loanedArea: Double) {
    override def toString = s"($size, ${ownedArea.toInt}, ${loaned}, ${loanedArea.toInt})"
  }

  type PopulationStats = Map[KitchenID, PopulationStat]
  type ParcelStats = Map[KitchenID, ParcelStat]
  type Loans = Seq[Loan]
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
                                    loans: Loans = Seq()
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
      val loans = yearHistory.loans.groupBy(_.from).map(x => x._1 -> x._2.map { y => s"${y.to} (${x._2.size})" }.mkString(","))
      val sortedPop = yearHistory.population.map { kp =>
        (kp._1, kp._2.size, kp._2.births, kp._2.emigrants, kp._2.absorbedKitchens, kp._2.splittedInto
        )
      }.toSeq.sortBy(_._1)
      val totalPop = sortedPop.map { x => Seq(x._2, x._3, x._4, x._5.size, x._6.map { _ => 1 }.getOrElse(0)) }.transpose.map {
        _.sum
      }
      val pStats = yearHistory.parcelStats


      val table = Seq(Seq("KID", "Owned Size/Area", "Loaned Size/Area", "Size", "Births", "Migs", "Absor", "Split")) ++ sortedPop.map(p =>
        Seq(
          p._1.toString,
          s"${pStats(p._1).size}, ${pStats(p._1).ownedArea.toInt}",
          s"${pStats(p._1).loaned}, ${pStats(p._1).loanedArea.toInt}",
          p._2.toString,
          p._3.toString,
          p._4.toString,
          p._5.mkString(","),
          p._6.map(_.toString).getOrElse("")
        )
      )

      println(s"PARCELS POPULATION ${totalPop(0)} / ${totalPop(1)} / ${totalPop(2)} KITCHENS ${totalPop(3)} / ${totalPop(4)}")
      if (verbose) println(Tabulator.formatTable(table))
    }
  }
}