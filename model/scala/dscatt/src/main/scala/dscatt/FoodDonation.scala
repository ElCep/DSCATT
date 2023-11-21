package dscatt

import dscatt.Croping.NotAssigned
import dscatt.Kitchen.{Food, FoodBalance, KitchenID}
import dscatt.Parcel.ParcelID
import dscatt.Simulation.SimulationState
import org.apache.commons.math3.random.MersenneTwister

import scala.annotation.tailrec

object FoodDonation {
  case class FoodDonation(from: KitchenID, to: KitchenID, quantity: Double)

  def assign(foods: Seq[Food], state: SimulationState): Seq[Food] = {

    @tailrec
    def assign0(hungryKitchens: List[FoodBalance], extraFoodKitchens: List[FoodBalance], foods: Seq[Food]): Seq[Food] = {
      if (hungryKitchens.isEmpty || extraFoodKitchens.isEmpty) foods
      else {
        val mostNeedy = hungryKitchens.head
        val donator = extraFoodKitchens.head

        val required = - mostNeedy.balance

        val (newHungryList, newExtraList, newFoodDonation) = donator.balance - required match {
          case f if f <= 0 =>
            (hungryKitchens.updated(0, mostNeedy.copy(balance = mostNeedy.balance + required)),
              extraFoodKitchens.tail,
              FoodDonation(donator.kitchenID, mostNeedy.kitchenID, donator.balance)
            )
          case _ =>
            (hungryKitchens.tail,
              extraFoodKitchens.updated(0, donator.copy(balance = donator.balance - required)),
              FoodDonation(donator.kitchenID, mostNeedy.kitchenID, required)
            )

        }
        
        val kitchenIndex = foods.indexWhere(f=> f.kitchenID == mostNeedy.kitchenID)
        val newFoods =
          if (kitchenIndex > -1)
            val currentFood = foods(kitchenIndex)
            foods.updated(kitchenIndex, currentFood.copy(fromDonation = currentFood.fromDonation + newFoodDonation.quantity))
          else foods

        assign0(newHungryList.sortBy(_.balance), newExtraList, newFoods)
      }
    }

    val foodBalances = foods.map{f=>
      FoodBalance(f.kitchenID, f.toBalance)
    }

    val (hungryKitchenBalances, extraFoodKitchenBalances) = foodBalances.partition(_.balance < 0)
    val altruistExtraFoodBalances = extraFoodKitchenBalances.filter{fb=>
      Kitchen.kitchen(state.kitchens, fb.kitchenID).map{_.foodDonationStrategy} == Some(FoodDonationStrategy.FoodForAllStrategy)
    }

    assign0(hungryKitchenBalances.sortBy(_.balance).toList, altruistExtraFoodBalances.toList, foods)
  }
}
