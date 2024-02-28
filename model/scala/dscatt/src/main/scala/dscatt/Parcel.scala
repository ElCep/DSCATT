package dscatt

import Croping.*
import Kitchen.*
import Parcel.*
import Data.*
import dscatt.Fertility.Metrics

object Parcel {
  type ParcelID = String

  case class ManureDeposit(year: Int, quantity: Double)

  def isCultivated(parcel: Parcel) = parcel.crop match {
    case Peanut | Millet => true
    case _ => false
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

implicit class ParcelDecorator(p: Parcel):
  def resetFertilityHistory: Parcel = p.copy(fertilityHistory = Seq())
