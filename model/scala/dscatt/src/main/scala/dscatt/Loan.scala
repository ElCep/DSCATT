package dscatt

import Kitchen.{FoodBalance, KitchenID}
import Croping.Crop.*

import scala.annotation.tailrec


case class Loan(from: KitchenID, to: KitchenID, parcel: Parcel)

object Loan:

  // Every loaned parcel will be used with Mil
  def assign(parcelsToBeLoaned: Array[Parcel], demandingKitchens: Array[FoodBalance], data: Data, year: Int): (Array[Loan], Array[Parcel]) = {

    @tailrec
    def assign0(demandingKitchens: Array[FoodBalance], availableParcels: Array[Parcel], yearLoans: Array[Loan]): (Array[Loan], Array[Parcel]) = {
      if (demandingKitchens.isEmpty || availableParcels.isEmpty) {
        (yearLoans, availableParcels)
      }

      else {
        val mostNeedy = demandingKitchens.head
        val currentParcel = availableParcels.head
        val loanedParcel = currentParcel.copy(farmerID = mostNeedy.kitchenID, crop = Millet, initiallyPlannedCrop = Some(currentParcel.crop))

        val newDemandingKitchens = demandingKitchens
          .updated(0, mostNeedy.copy(balance = mostNeedy.balance + Kitchen.parcelFoodProduction(loanedParcel, data, year)))
          .sortBy(_.balance)
          .filter(_.balance < 0)
        assign0(newDemandingKitchens, availableParcels.tail, yearLoans :+ Loan(loanedParcel.ownerID, mostNeedy.kitchenID, loanedParcel))
      }

    }
    assign0(demandingKitchens.sortBy(_.balance), parcelsToBeLoaned, Array[Loan]())
  }


  def reset(world: World) =
    world.copy(
      parcels = world.parcels.map: p =>
        p.copy(
          farmerID = p.ownerID,
          crop = p.initiallyPlannedCrop.getOrElse(p.crop),
          initiallyPlannedCrop = None
        )
    )
