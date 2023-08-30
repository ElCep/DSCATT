package dscatt

import dscatt.FoodDonation.FoodDonation
import dscatt.Kitchen.{FoodBalance, KitchenID}
import dscatt.Loan.Loan
import dscatt.Simulation.SimulationState

object History {

  case class PopulationStat(size: Int, births: Int, emigrants: Int, herdSize: Int, absorbedKitchens: Seq[KitchenID], splittedInto: Option[KitchenID])

  case class ParcelStat(size: Int, ownedArea: Double, loaned: Int, loanedArea: Double) {
    override def toString = s"($size, ${ownedArea.toInt}, ${loaned}, ${loanedArea.toInt})"
  }

  case class FoodBalanceOverYear(initialFoodNeeds: Int = 0, autonomousFoodBalance: Int = 0, afterLoanFoodBalance: Int = 0, afterDonationFoodBalance: Int = 0)

  type PopulationStats = Map[KitchenID, PopulationStat]
  type ParcelStatsByKitchen = Map[KitchenID, ParcelStat]
  type Loans = Seq[Loan]
  type FoodBalanceStats = Map[KitchenID, FoodBalanceOverYear]
  type History = Map[Int, YearHistory]
  type Fertilities = Map[KitchenID, Fertility.Metrics]

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
  }

  protected case class YearHistory(
                                    year: Int,
                                    population: PopulationStats = Map(),
                                    parcelStats: ParcelStatsByKitchen = Map(),
                                    loans: Loans = Seq(),
                                    foodBalanceStats: FoodBalanceStats = Map(),
                                    fertilities: Fertilities = Map()
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

  def printKitckens(state: SimulationState, verbose: Boolean = false) = {
    state.history.keys.toSeq.sorted.foreach { y =>
      val yearHistory = state.history(y)
      println(s"\nYEAR $y\n")
      val sortedPop = yearHistory.population.map { kp =>
        (kp._1, kp._2.size, kp._2.births, kp._2.emigrants, kp._2.herdSize, kp._2.absorbedKitchens, kp._2.splittedInto
        )
      }.toSeq.sortBy(_._1)
      val totalPop = sortedPop.map { x => Seq(x._2, x._3, x._4, x._5, x._6.size, x._7.map { _ => 1 }.getOrElse(0)) }.transpose.map {
        _.sum
      }
      val pStats = yearHistory.parcelStats
      val fbStats = yearHistory.foodBalanceStats
      val fertilityStats = yearHistory.fertilities
      val doubleFormat = "%.5f"
      val intFormat = "%.0f"

      val table = Seq(Seq("KID", "Owned Size/Area", "Loaned Size/Area", "Herd", "Man/Mulch/N/SQ", "Food Balance (initFN/AFB/ALFB/ADFB)", "Size", "Births", "Migs", "Absor", "Split")) ++ sortedPop.map { p =>
        val fbStatsK = fbStats.getOrElse(p._1, FoodBalanceOverYear())
        val fertilityStatK = fertilityStats.getOrElse(p._1, Fertility.Metrics(state.year))
        Seq(
          p._1.toString,
          s"${pStats(p._1).size}, ${pStats(p._1).ownedArea.toInt}",
          s"${pStats(p._1).loaned}, ${pStats(p._1).loanedArea.toInt}",
          s"${p._5}",
          s"${intFormat.format(fertilityStatK.manureMass)}, ${intFormat.format(fertilityStatK.mulchingMass)}, ${intFormat.format(fertilityStatK.agronomicMetrics.availableNitrogen)}, ${intFormat.format(fertilityStatK.agronomicMetrics.soilQuality)} ",
          s"${fbStatsK.initialFoodNeeds}, ${fbStatsK.autonomousFoodBalance}, ${fbStatsK.afterLoanFoodBalance}, ${fbStatsK.afterDonationFoodBalance}",
          p._4.toString,
          p._5.toString,
          p._5.toString,
          p._6.mkString(","),
          p._7.map(_.toString).getOrElse("")
        )
      }

      println(s"PARCELS ${pStats.map(_._2.size).sum} POPULATION ${totalPop(0)} / ${totalPop(1)} / ${totalPop(2)} KITCHENS ${totalPop(3)} / ${totalPop(4)}")
      if (verbose) println(Tabulator.formatTable(table))
    }
  }

  def printParcels(state: SimulationState) = {
    val first20 = state.world.parcels.take(20)

    val doubleFormat = "%.5f"
    val intFormat = "%.0f"

    state.history.keys.toSeq.sorted.foreach { y =>
      println(s"\nYEAR $y\n")
      val table = Seq(Seq("ID", "Area", "Loaned?", "QS", "N", "Manure", "Mulch", "Yield/ha", "for crop")) ++
        first20.map {p =>
          val fertility = p.fertilityHistory(y - 1)
          val previousYearAgronomicMetrics = {
            if (y > 1) Some(y - 2)
            else None
          }.map { o => p.fertilityHistory(o).agronomicMetrics }
          Seq(
            p.tinyID,
            doubleFormat.format(p.area),
            (p.farmerID != p.ownerID).toString,
            doubleFormat.format(fertility.agronomicMetrics.soilQuality),
            doubleFormat.format(fertility.agronomicMetrics.availableNitrogen),
            doubleFormat.format(fertility.manureMass),
            doubleFormat.format(fertility.mulchingMass),
            previousYearAgronomicMetrics.map { amet =>
              doubleFormat.format(Kitchen.parcelFoodProduction(p, amet) / p.area)
            }.getOrElse("N/A"),
            fertility.crop.display
          )
        }
      println(Tabulator.formatTable(table))
    }
  }


}