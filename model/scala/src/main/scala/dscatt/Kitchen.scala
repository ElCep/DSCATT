package dscatt

import breeze.stats.distributions.{Gaussian, RandBasis, ThreadLocalRandomGenerator}
import dscatt.Croping._
import org.apache.commons.math3.random.MersenneTwister

object Kitchen {

  type KitchenID = Int

  def parcelsOfTheYear(kitchenID: KitchenID): Seq[Parcel] = ???

  def buildKitchens(numberOfKitchens: Int, sizeAverage: Double, sizeStd: Double)(using mT: MersenneTwister): Seq[Kitchen] = {
    val randBasis = new RandBasis(new ThreadLocalRandomGenerator(mT))
    val gaussian = Gaussian(sizeAverage, sizeStd)(randBasis)
    gaussian.samples.take(numberOfKitchens).toArray.zipWithIndex.map { case (s, i) => Kitchen(i + 1, s.toInt) }
  }

  def foodNeeds(kitchen: Kitchen) =  kitchen.size * Constants.DAILY_FOOD_NEED_PER_PERSON * 365

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
}

case class Kitchen(id: Kitchen.KitchenID, size: Int)
