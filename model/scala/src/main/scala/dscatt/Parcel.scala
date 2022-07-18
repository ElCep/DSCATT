package dscatt

import dscatt.Croping.Rotation
import dscatt.Kitchen.*
import dscatt.Parcel.*

object Parcel {
  type ParcelID = String
}

case class Parcel(id: ParcelID,
                  kitchenID: KitchenID,
                  rotation: Rotation,
                  area: Double,
                  distanceToVillage: Double,
                  neighbours: Seq[ParcelID])
