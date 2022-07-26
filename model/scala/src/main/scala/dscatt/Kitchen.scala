package dscatt

import breeze.stats.distributions.{Gaussian, RandBasis, ThreadLocalRandomGenerator}
import dscatt.Croping.*
import org.apache.commons.math3.random.MersenneTwister

import scala.annotation.tailrec

object Kitchen {

  type KitchenID = Int

  def parcelsOfTheYear(kitchenID: KitchenID): Seq[Parcel] = ???

  def buildKitchens(numberOfKitchens: Int, sizeAverage: Double, sizeStd: Double)(using mT: MersenneTwister): Seq[Kitchen] = {
    val randBasis = new RandBasis(new ThreadLocalRandomGenerator(mT))
    val gaussian = Gaussian(sizeAverage, sizeStd)(randBasis)
    gaussian.samples.take(numberOfKitchens).toArray.zipWithIndex.map { case (s, i) => Kitchen(i + 1, s.toInt) }
  }

  def foodNeeds(kitchen: Kitchen) = kitchen.size * Constants.DAILY_FOOD_NEED_PER_PERSON * 365

  def cultivatedSurface(world: World, kitchen: Kitchen): Double = {
    val cultivatedParcels = World.parcelsForKitchen(world, kitchen).filter(p => Parcel.isCultivated(p))
    cultivatedParcels.map(_.area).sum
  }

  //  def production(world: World, kitchen: Kitchen) = {
  //
  //    val parcels = World.parcelsForKitchen(world, kitchen)
  //    parcelsProduction(parcels)
  //  }

  def parcelProduction(parcel: Parcel) = {
    parcel.crop match {
      case Mil => parcel.area * Constants.MIL_YIELD_PER_M2
      case Peanut => parcel.area * Constants.PEANUT_YIELD_PER_M2 * Constants.PEANUT_FOOD_EQUIVALENCE
      case _ => 0.0
    }
  }

  def parcelsProduction(parcels: Seq[Parcel]) = {
    parcels.map {
      parcelProduction
    }.sum
  }

  def foodBalance(world: World, kitchen: Kitchen): Double = {
    //FIXME: should be cultivated surface
    parcelsProduction(World.parcelsInCultureForKitchen(world, kitchen)) - foodNeeds(kitchen)
  }


  def evolve(world: World, kitchens: Seq[Kitchen], populationGrowth: Double)(using mT: MersenneTwister) = {


    println("## EVOLVE KITCHENS " + kitchens.size)
    println("## TOTAL POP " + kitchens.map{_.size}.sum)
    val updatedSizeKitchens = evolvePopulation(world, kitchens, populationGrowth)
    val regorganiszedKitchens = kitchenAbsorption(updatedSizeKitchens)
    regorganiszedKitchens
    // kitchenSplit
  }

  // Compute for each kitchen the number of births and the number of emigrants based on the food balance
  def evolvePopulation(world: World, kitchens: Seq[Kitchen], populationGrowth: Double) = {

    println("EVOLVE POP " + kitchens.map{_.size}.mkString(" // "))
    kitchens.map { k =>
      val balanceK = foodBalance(world, k)
      if (balanceK > 0) {
        val nbBirths = (populationGrowth * k.size).floor.toInt
        k.copy(size = k.size + nbBirths, birthPerYear = k.birthPerYear :+ nbBirths, emigrantsPerYear = k.emigrantsPerYear :+ 0)
      }
      else {
        val nbEmigrants = (Math.abs(balanceK) / (Constants.DAILY_FOOD_NEED_PER_PERSON * 365)).ceil.toInt
        k.copy(size = (k.size - nbEmigrants), birthPerYear = k.birthPerYear :+ 0, emigrantsPerYear = k.emigrantsPerYear :+ nbEmigrants)
      }
    }
  }

  def kitchenAbsorption(kitchens: Seq[Kitchen])(using mT: MersenneTwister): Seq[Kitchen] = {

    val toBeAbsorbedKitcken = kitchens.filter(k => k.size < Constants.KITCHEN_MINIMUM_SIZE)
    val absorbingKitchens = kitchens.diff(toBeAbsorbedKitcken)

    println("tO BE ABSORBED " + toBeAbsorbedKitcken.length)
    println("ABSORBING " + absorbingKitchens.length)

    @tailrec
    def absorption(toBeAbsorbed: Seq[Kitchen], absorbing: Seq[Kitchen]): Seq[Kitchen] = {
      if (toBeAbsorbed.length == 0) absorbing
      else {
        val index = mT.nextInt(absorbing.length - 1)
        val oneAbsorbing = absorbing(index)
        val newAbsorbing = absorbing.updated(index, oneAbsorbing.copy(size = oneAbsorbing.size + toBeAbsorbed.head.size))
        println("KITCHEN " + oneAbsorbing.id + " is absorbing kitchen " + toBeAbsorbed.head.id)
        absorption(toBeAbsorbed.tail, newAbsorbing)
      }
    }

    if (absorbingKitchens.length > 0) absorption(toBeAbsorbedKitcken, absorbingKitchens).diff(toBeAbsorbedKitcken)
    else kitchens
  }

}

case class Kitchen(id: Kitchen.KitchenID, size: Int, birthPerYear: Seq[Int] = Seq(), emigrantsPerYear: Seq[Int] = Seq())
