package dscatt

import dscatt.Kitchen.{FoodBalance, KitchenID, foodBalance}
import org.apache.commons.math3.random.MersenneTwister

import scala.annotation.tailrec

object Population {

  private def cumSum[A](xs: Seq[A])(implicit num: Numeric[A]): Seq[A] = {
    xs.tail.scanLeft(xs.head)(num.plus)
  }

  def evolve(kitchens: Seq[Kitchen], populationGrowth: Double)(using mT: MersenneTwister): Seq[Kitchen] = {
    val totalPopulation = kitchens.map {
      _.size
    }.sum
    val nbBirths = (totalPopulation * populationGrowth).floor.toInt

    println("NBBIRTG " + nbBirths)

    //Uniform distribution for births
    val weight = 1.0 / kitchens.size
    val cumulatedWeights = kitchens.map(_.id).zip(cumSum(Seq.fill(kitchens.size)(weight)))

    @tailrec
    def findIndex(rnd: Double, index: Int): KitchenID = {
      if(index >= kitchens.size - 1 || rnd < cumulatedWeights(index)._2) cumulatedWeights(index)._1
      else findIndex(rnd, index + 1)
    }

    val nbBirthByKitchen = (1 to nbBirths).map { i =>
      val rnd = mT.nextDouble()
      findIndex(rnd, 0)
    }.groupBy(identity).map{x=> x._1-> x._2.size}


    val kitchenIDsToBeUpdated = nbBirthByKitchen.keys.toSeq
    val (toBeEvolved, noEvolution) = kitchens.partition(k=> kitchenIDsToBeUpdated.contains(k.id))

    println(nbBirthByKitchen)
    toBeEvolved.map{k=>
      val nbBirthK = nbBirthByKitchen(k.id)
      k.copy(size = k.size + nbBirthK, birthPerYear = k.birthPerYear :+ nbBirthK)
    } ++ noEvolution.map{k=> k.copy(birthPerYear = k.birthPerYear :+ 0)}
  }

  // Compute for each kitchen the number of births and the number of emigrants based on the food balance
  def evolveEmigrants(world: World, kitchens: Seq[Kitchen], foodAssessment: Seq[FoodBalance]): Seq[Kitchen] = {

    val foodAssessementMap = foodAssessment.map(fa=> fa.kitchenID-> fa.balance).toMap
    
    kitchens.map { k =>
      val balanceK = foodAssessementMap(k.id)
      if (balanceK > 0) {
        k.copy(emigrantsPerYear = k.emigrantsPerYear :+ 0)
      }
      else {
        val theoriticalNbEmigrants = (Math.abs(balanceK) / (Constants.DAILY_FOOD_NEED_PER_PERSON * 365)).ceil.toInt
        
        // The emigration process can't remove entirely the kitchen. It should be split into migration
        // (up to KITCHEN_MINIMUM_SIZE remaining in kitchen) and kitchen absorbtion
        val nbEmigrants = {
          val emigrantThreshold = k.size - Constants.KITCHEN_MINIMUM_SIZE
          if (theoriticalNbEmigrants <= emigrantThreshold) theoriticalNbEmigrants
          else emigrantThreshold
        }
        // println("BALANCE " + k.size  + " -- " + nbEmigrants + " == " + (k.size - nbEmigrants))
        k.copy(size = (k.size - nbEmigrants), emigrantsPerYear = k.emigrantsPerYear :+ nbEmigrants)
      }
    }
  }
}
