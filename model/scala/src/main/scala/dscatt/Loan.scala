package dscatt

import dscatt.Croping.NotAssigned
import dscatt.Kitchen.{FoodBalance, KitchenID}
import dscatt.Parcel.ParcelID
import org.apache.commons.math3.random.MersenneTwister

import scala.annotation.tailrec


object Loan {
  case class Loan(year: Int, from: KitchenID, to: KitchenID, parcel: Parcel)

  case class LoanHistory(records: Seq[Loan]) {
    def append(loanHistory: LoanHistory) = copy(records = records ++ loanHistory.records)
  }

  def assign(year: Int, parcelsToBeLoaned: Seq[Parcel], demandingKitchens: Seq[FoodBalance], loanHistory: LoanHistory): (LoanHistory, Seq[Parcel]) = {

    @tailrec
    def assign0(demandingKitchens: List[FoodBalance], availableParcels: Seq[Parcel], loanHistory: LoanHistory): (LoanHistory, Seq[Parcel]) = {
      if (demandingKitchens.isEmpty || availableParcels.isEmpty) (loanHistory, availableParcels)
      
      else {
        val mostNeedy = demandingKitchens.head
        val loanedParcel = availableParcels.head
        val newDemandingKitchens = demandingKitchens
          .updated(0, mostNeedy.copy(balance = mostNeedy.balance + Kitchen.parcelProduction(loanedParcel)))
          .sortBy(_.balance)
          .filter(_.balance < 0)
        assign0(newDemandingKitchens, availableParcels.tail, loanHistory.copy(records = loanHistory.records :+ Loan(year, loanedParcel.ownerID, mostNeedy.kitchenID, loanedParcel)))
      }

    }

    assign0(demandingKitchens.sortBy(_.balance).toList, parcelsToBeLoaned.toList, loanHistory)
  }


  def reset(world: World) = world.copy(parcels = world.parcels.map(p => p.copy(farmerID = p.ownerID)))

}
