package dscatt

import dscatt.Croping.NotAssigned
import dscatt.Kitchen.{FoodBalance, KitchenID}
import dscatt.Parcel.ParcelID
import dscatt.Simulation.SimulationState
import org.apache.commons.math3.random.MersenneTwister
import dscatt.Croping.AParcel

import scala.annotation.tailrec


object Loan {
  case class Loan(from: KitchenID, to: KitchenID, parcel: Parcel)

  def assign(year: Int, parcelsToBeLoaned: Seq[Parcel], demandingKitchens: Seq[FoodBalance]): (Seq[Loan], Seq[Parcel]) = {

    @tailrec
    def assign0(demandingKitchens: List[FoodBalance], availableParcels: Seq[Parcel], yearLoans: Seq[Loan]): (Seq[Loan], Seq[Parcel]) = {
      if (demandingKitchens.isEmpty || availableParcels.isEmpty) {
        (yearLoans, availableParcels)
      }

      else {
        val mostNeedy = demandingKitchens.head
        val loanedParcel = availableParcels.head
        val newDemandingKitchens = demandingKitchens
          .updated(0, mostNeedy.copy(balance = mostNeedy.balance + Kitchen.parcelProductionForLoan(loanedParcel)))
          .sortBy(_.balance)
          .filter(_.balance < 0)
        assign0(newDemandingKitchens, availableParcels.tail, yearLoans :+ Loan(loanedParcel.ownerID, mostNeedy.kitchen.id, loanedParcel))
      }

    }

    assign0(demandingKitchens.sortBy(_.balance).toList, parcelsToBeLoaned.toList, Seq())
  }


  def reset(world: World) = world.copy(parcels = world.parcels.map(p => p.copy(farmerID = p.ownerID).setFallowIfCropZoneThree))

}
