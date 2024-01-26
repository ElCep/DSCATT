package dscatt

import Kitchen.{FoodBalance, KitchenID}
import Parcel.ParcelID
import Simulation.SimulationState
import Croping.AParcel
import Constants.*

import scala.annotation.tailrec


case class Loan(from: KitchenID, to: KitchenID, parcel: Parcel)

object Loan {

  def assign(parcelsToBeLoaned: Seq[Parcel], demandingKitchens: Seq[FoodBalance])(using Fertility.AgronomicMetricsByParcel, MM): (Seq[Loan], Seq[Parcel]) = {

    @tailrec
    def assign0(demandingKitchens: List[FoodBalance], availableParcels: Seq[Parcel], yearLoans: Seq[Loan]): (Seq[Loan], Seq[Parcel]) = {
      if (demandingKitchens.isEmpty || availableParcels.isEmpty) {
        (yearLoans, availableParcels)
      }

      else {
        val mostNeedy = demandingKitchens.head
        val loanedParcel = availableParcels.head
        val newDemandingKitchens = demandingKitchens
          .updated(0, mostNeedy.copy(balance = mostNeedy.balance + Kitchen.parcelFoodProductionForLoan(loanedParcel)))
          .sortBy(_.balance)
          .filter(_.balance < 0)
        assign0(newDemandingKitchens, availableParcels.tail, yearLoans :+ Loan(loanedParcel.ownerID, mostNeedy.kitchenID, loanedParcel))
      }

    }

    assign0(demandingKitchens.sortBy(_.balance).toList, parcelsToBeLoaned.toList, Seq())
  }


  def reset(world: World) = world.copy(parcels = world.parcels.map(p => p.copy(farmerID = p.ownerID).updateCrops))

}
