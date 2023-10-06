package dscatt

import dscatt.Croping._
import dscatt.Kitchen._
import dscatt.Parcel._
import dscatt.Constants._

object Parcel {
  type ParcelID = String

  case class ManureDeposit(year: Int, quantity: Double)
  
  def isCultivated(parcel: Parcel) = parcel.crop match {
    case Peanut | Mil=> true
    case _=> false
  }
  
  def isAssigned(parcel: Parcel) = parcel.crop != NotAssigned
}

implicit class AParcel(parcel: Parcel) {
  def tinyID = parcel.id.take(11)
}

case class Parcel(id: ParcelID,
                  ownerID: KitchenID,
                  farmerID: KitchenID,
                  cropZone: CropZone,
                  crop: Crop,
                  area: HA,
                  faidherbiaTrees: Int,
                  fertilityHistory: Seq[Fertility.Metrics]
                 )

