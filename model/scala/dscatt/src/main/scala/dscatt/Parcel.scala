package dscatt

import Croping._
import Kitchen._
import Parcel._
import Constants._

object Parcel {
  type ParcelID = String

  case class ManureDeposit(year: Int, quantity: Double)
  
  def isCultivated(parcel: Parcel) = parcel.crop match {
    case Peanut | Mil=> true
    case _=> false
  }
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
                  faidherbiaTrees: Double,
                  fertilityHistory: Seq[Fertility.Metrics]
                 )

