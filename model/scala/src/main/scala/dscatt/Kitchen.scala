package dscatt

import breeze.stats.distributions.{Gaussian, RandBasis, ThreadLocalRandomGenerator}
import dscatt.Croping.*
import org.apache.commons.math3.random.MersenneTwister

import scala.annotation.tailrec

object Kitchen {

  type KitchenID = Int

  def parcelsOfTheYear(kitchenID: KitchenID): Seq[Parcel] = ???

  def buildKitchens(kitchenPartition: KitchenPartition): Seq[Kitchen] = {
   // val randBasis = new RandBasis(new ThreadLocalRandomGenerator(mT))
   // val gaussian = Gaussian(sizeAverage, sizeStd)(randBasis)
   // gaussian.samples.take(numberOfKitchens).toArray.zipWithIndex.map { case (s, i) => Kitchen(i + 1, s.toInt) }
   kitchenPartition.profiles.flatMap{p=>Seq.fill[KitchenProfile](p._2)(p._1)}.zipWithIndex.map{case (kp,id)=>
     Kitchen(id + 1, kp.size, kp.rotationCycle, kp.cropingStrategy, kp.loanStrategy)
   }
  }

  def kitchen(kitchens: Seq[Kitchen], id: KitchenID) = kitchens.find(_.id == id)

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

    println("## TOTAL POP " + kitchens.map {
      _.size
    }.sum)

    val updatedSizeKitchens = evolvePopulation(world, kitchens, populationGrowth)
    val (regorganiszedKitchens, reorganizedWorld) = kitchenAbsorption(updatedSizeKitchens, world)
    (regorganiszedKitchens, reorganizedWorld)
    // kitchenSplit
  }

  // Compute for each kitchen the number of births and the number of emigrants based on the food balance
  def evolvePopulation(world: World, kitchens: Seq[Kitchen], populationGrowth: Double) = {

    kitchens.map { k =>
      val balanceK = foodBalance(world, k)
      if (balanceK > 0) {
        val nbBirths = (populationGrowth * k.size).ceil.toInt
        k.copy(size = k.size + nbBirths, birthPerYear = k.birthPerYear :+ nbBirths, emigrantsPerYear = k.emigrantsPerYear :+ 0)
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
        k.copy(size = (k.size - nbEmigrants), birthPerYear = k.birthPerYear :+ 0, emigrantsPerYear = k.emigrantsPerYear :+ nbEmigrants)
      }
    }
  }

  def kitchenAbsorption(kitchens: Seq[Kitchen], world: World)(using mT: MersenneTwister): (Seq[Kitchen], World) = {

    case class AbsorbingKitchen(kitchen: Kitchen, absorbedIDs: Seq[KitchenID])

    val toBeAbsorbedKitcken = kitchens.filter(k => k.size <= Constants.KITCHEN_MINIMUM_SIZE)
    val absorbingKitchens = kitchens.diff(toBeAbsorbedKitcken).map { k => AbsorbingKitchen(k, Seq()) }

    println("tO BE ABSORBED " + toBeAbsorbedKitcken.length)
    println("ABSORBING " + absorbingKitchens.length)


    @tailrec
    def absorption(toBeAbsorbed: Seq[Kitchen], absorbing: Seq[AbsorbingKitchen]): Seq[AbsorbingKitchen] = {
      val nbAbsorbing = absorbing.length
      if (toBeAbsorbed.length == 0 || nbAbsorbing < 1) absorbing
      else {
        val index = mT.nextInt(nbAbsorbing)
        val oneAbsorbing = absorbing(index)
        val newAbsorbing = absorbing.updated(index,
          oneAbsorbing.copy(
            kitchen = oneAbsorbing.kitchen.copy(size = oneAbsorbing.kitchen.size + toBeAbsorbed.head.size),
            absorbedIDs = oneAbsorbing.absorbedIDs :+ toBeAbsorbed.head.id
          )
        )
        println("KITCHEN " + oneAbsorbing.kitchen.id + " is absorbing kitchen " + toBeAbsorbed.head.id)
        absorption(toBeAbsorbed.tail, newAbsorbing)
      }
    }

    val reorganizedKitchens = absorption(toBeAbsorbedKitcken, absorbingKitchens)
    val reorganizedWorld = {
      // get the match between each absorbed id and its absorbing id
      val absorbedMap = reorganizedKitchens.flatMap(k => k.absorbedIDs.map(_ -> k.kitchen.id)).toMap

      world.copy(parcels = world.parcels.map { p =>
        p.copy(kitchenID = absorbedMap.getOrElse(p.kitchenID, p.kitchenID))
      })
    }

    (reorganizedKitchens.map(_.kitchen), reorganizedWorld)
  }
}

case class Kitchen(id: Kitchen.KitchenID,
                   size: Int,
                   rotationCycle: RotationCycle,
                   cropingStrategy: CropingStrategy,
                   loanStrategy: LoanStrategy,
                   birthPerYear: Seq[Int] = Seq(),
                   emigrantsPerYear: Seq[Int] = Seq()
                  )