package dscatt

import Croping.Crop
import Croping.Crop.*
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
                  //cropZone: CropZone,
                  crop: Crop,
                  initiallyPlannedCrop: Option[Crop], // set in case of loan
                  area: HA,
                  faidherbiaTreesByHa: TREE_BY_HA,
                  fertilityHistory: Array[Fertility.Metrics]
                 )

implicit class ParcelDecorator(p: Parcel):
  def resetFertilityHistory: Parcel = p.copy(fertilityHistory = Array())
