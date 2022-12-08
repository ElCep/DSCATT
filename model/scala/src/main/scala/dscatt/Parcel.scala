package dscatt

import dscatt.Croping._
import dscatt.Kitchen._
import dscatt.Parcel._

object Parcel {
  type ParcelID = String


  case class ManureDeposit(year: Int, quantity: Double)
  
  def isCultivated(parcel: Parcel) = parcel.crop match {
    case Peanut | Mil=> true
    case _=> false
  }
}

case class Parcel(id: ParcelID,
                  ownerID: KitchenID,
                  farmerID: KitchenID,
                  cropZone: CropZone,
                  crop: Crop,
                  area: Double,
                  distanceToVillage: Double,
                  neighbours: Seq[ParcelID],
                  fedherbiaTrees: Int,
                  soilQuality: Double,
                  manureDeposits: Seq[ManureDeposit])
