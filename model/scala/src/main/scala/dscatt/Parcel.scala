package dscatt

import dscatt.Kitchen._
import dscatt.Parcel._

object Parcel {
  type ParcelID = String

  sealed trait Crop
  object Mil extends Crop
  object Peanut extends Crop
  object Fallow extends Crop
  object NotAssigned extends Crop

  sealed trait CropZone
  object One extends CropZone
  object Two extends CropZone
  object Three extends CropZone
  object Village extends CropZone

  implicit def intToCropZone(cz: Int): CropZone = cz match
    case 1=> One
    case 2=> Two
    case 3=> Three
}

case class Parcel(id: ParcelID,
                  kitchenID: KitchenID,
                  crop: Crop = NotAssigned,
                  cropZone: CropZone,
                  area: Double,
                  distanceToVillage: Double,
                  neighbours: Seq[ParcelID])
