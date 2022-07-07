package dscatt


object Kitchen {

  type KitchenID = Int

  def parcelsOfTheYear(kitchenID: KitchenID): Seq[Parcel] = ???

}


import dscatt.Kitchen._
import dscatt.Parcel._

case class Kitchen(id: KitchenID)
