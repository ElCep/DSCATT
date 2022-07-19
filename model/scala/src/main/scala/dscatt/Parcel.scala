package dscatt

import dscatt.Croping._
import dscatt.Kitchen._
import dscatt.Parcel._

object Parcel {
  type ParcelID = String

  def isCultivated(parcel: Parcel) = parcel.crop match {
    case Peanut | Mil=> true
    case _=> false
  }
}

case class Parcel(id: ParcelID,
                  kitchenID: KitchenID,
                  cropZone: CropZone,
                  crop: Crop,
                  area: Double,
                  distanceToVillage: Double,
                  neighbours: Seq[ParcelID])
