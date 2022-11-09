package dscatt

import dscatt.Kitchen.KitchenID
import dscatt.Loan.Loan
import dscatt.Simulation.SimulationState

object History {

  case class Population(size: Int, births: Int, emigrants: Int, absorbedKitchens: Seq[KitchenID], splittedInto: Option[KitchenID])

  type KitchenPopulations = Map[KitchenID, Population]
  type Loans = Seq[Loan]
  type History = Map[Int, YearHistory]

  def initialize(simulationLenght: Int, kitchens: Seq[Kitchen]): History = {
    val history = (0 to simulationLenght).map { y =>
      y -> YearHistory(year = y)
    }.toMap
    history.updated(0, YearHistory(year = 0, population = kitchens.map { k => k.id -> Population(k.size, 0, 0, Seq(), None) }.toMap))
  }

  implicit class HistoryWrap(history: History) {
    def updateLoans(year: Int, loans: Loans) = {
      val historyOfYear = history(year)
      history.updated(year, historyOfYear.copy(loans = historyOfYear.loans ++ loans))
    }

    def updatePopulations(year: Int, populations: Seq[(KitchenID, Population)]): History = {
      val historyOfYear = history(year)
      history.updated(year, historyOfYear.copy(population = populations.toMap))
    }
  }

  protected case class YearHistory(
                                    year: Int,
                                    fertility: Double = 0.0,
                                    population: KitchenPopulations = Map(),
                                    loans: Loans = Seq()
                                  )


  def print(history: History, state: SimulationState, verbose: Boolean = false) = {
    history.keys.toSeq.sorted.foreach { y =>
      val yearHistory = history(y)
      println(s"\nYEAR $y\n")
      val sortedPop = yearHistory.population.map { kp =>
        (kp._1, kp._2.size, kp._2.births, kp._2.emigrants, kp._2.absorbedKitchens, kp._2.splittedInto
        )
      }.toSeq.sortBy(_._1)
      val totalPop = sortedPop.map { x => Seq(x._2, x._3, x._4, x._5.size, x._6.map { _ => 1 }.getOrElse(0)) }.transpose.map {
        _.sum
      }
      println(s"KID\t|\t\tPOPULATION ${totalPop(0)} / ${totalPop(1)} / ${totalPop(2)}\t\t|\t\tKITCHENS ${totalPop(3)} / ${totalPop(4)}\t\t")
      println("-------------------------------------------------------------------------------------------------")
      if (verbose) {
        val printable = sortedPop.map( p => (p._1.toString, p._2.toString, p._3.toString, p._4.toString, p._5.mkString(","), p._6.map(_.toString).getOrElse("\t")))
        println("\t|\tSize\t|\tBirths\t|\tMigs \t|\tAbsed\t|\tSplitted\t")
        printable.map(t => t.productIterator.mkString("\t|\t")).foreach(println)
      }
      println
    }
  }
}