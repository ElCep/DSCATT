package dscatt

import breeze.stats.distributions.{Gaussian, RandBasis, ThreadLocalRandomGenerator}
import org.apache.commons.math3.random.MersenneTwister

object Kitchen {

  type KitchenID = Int

  def parcelsOfTheYear(kitchenID: KitchenID): Seq[Parcel] = ???

  def buildKitchens(numberOfKitchens: Int, sizeAverage: Double, sizeStd: Double)(using mT: MersenneTwister): Seq[Kitchen] = {
    val randBasis = new RandBasis(new ThreadLocalRandomGenerator(mT))
    val gaussian = Gaussian(sizeAverage, sizeStd)(randBasis)
    gaussian.samples.take(numberOfKitchens).toArray.zipWithIndex.map{case (s, i)=> Kitchen(i+1, s.toInt)}
  }
}

case class Kitchen(id: Kitchen.KitchenID, size: Int)
