package dscatt

import dscatt.Croping.NotAssigned
import dscatt.Kitchen.{FoodBalance, KitchenID}
import dscatt.Parcel.ParcelID
import dscatt.Simulation.SimulationState
import org.apache.commons.math3.random.MersenneTwister

import scala.annotation.tailrec

object FoodDonation {
  case class FoodDonation(from: KitchenID, to: KitchenID, quantity: Double)

  def assign(foodBalances: Seq[FoodBalance]): Seq[FoodBalance] = {

    @tailrec
    def assign0(hungryKitchens: List[FoodBalance], extraFoodKitchens: List[FoodBalance], foodBalancesMap: Map[KitchenID, FoodBalance]): Seq[FoodBalance] = {
      if (hungryKitchens.isEmpty || extraFoodKitchens.isEmpty) foodBalancesMap.values.toSeq
      else {
        val mostNeedy = hungryKitchens.head
        val donator = extraFoodKitchens.head
       // val foodPotential = donator.balance

        val required = - mostNeedy.balance

        val (newHungryList, newExtraList, newFoodDonation) = donator.balance - required match {
          case f if f <= 0 =>
            (hungryKitchens.updated(0, mostNeedy.copy(balance = mostNeedy.balance + required)),
              extraFoodKitchens.tail,
              FoodDonation(donator.kitchen.id, mostNeedy.kitchen.id, donator.balance)
            )
          case _ =>
            (hungryKitchens.tail,
              extraFoodKitchens.updated(0, donator.copy(balance = donator.balance - required)),
              FoodDonation(donator.kitchen.id, mostNeedy.kitchen.id, required)
            )

        }

       // mostNeedy.balance is always negative
        val gift = donator.balance + mostNeedy.balance
        val newFoodBalances =
          foodBalancesMap
            .updated(mostNeedy.kitchen.id, mostNeedy.copy(balance = mostNeedy.balance + newFoodDonation.quantity))
            .updated(donator.kitchen.id, donator.copy(balance = donator.balance - newFoodDonation.quantity))

        assign0(newHungryList.sortBy(_.balance), newExtraList, newFoodBalances)
      }
    }


    val (hungryKitchenBalances, extraFoodKitchenBalances) = foodBalances.partition(_.balance < 0)
    val altruistExtraFoodBalances = extraFoodKitchenBalances.filter(_.kitchen.foodDonationStrategy == FoodForAllStrategy)

    assign0(hungryKitchenBalances.sortBy(_.balance).toList, altruistExtraFoodBalances.toList, foodBalances.map { b => b.kitchen.id -> b }.toMap)
  }
}
