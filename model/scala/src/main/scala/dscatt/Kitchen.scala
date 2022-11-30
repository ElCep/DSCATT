package dscatt

import breeze.stats.distributions.{Gaussian, RandBasis, ThreadLocalRandomGenerator}
import dscatt.Croping.*
import dscatt.Simulation.SimulationState
import org.apache.commons.math3.random.MersenneTwister

import scala.annotation.tailrec

object Kitchen {

  type KitchenID = Int

  case class FoodBalance(kitchen: Kitchen, balance: Double)

  def buildKitchens(kitchenPartition: KitchenPartition): Seq[Kitchen] = {
    kitchenPartition.profiles.flatMap { p => Seq.fill[KitchenProfile](p._2)(p._1) }.zipWithIndex.map { case (kp, id) =>
      Kitchen(id + 1, kp.size, kp.rotationCycle, kp.cropingStrategy, kp.loanStrategy, kp.foodDonationStrategy, kp.herdSize, kp.drySeasonHerdStrategy, kp.wetSeasonHerdStrategy, kp.fertilizerStrategy)
    }
  }

  def kitchen(kitchens: Seq[Kitchen], id: KitchenID) = kitchens.find(_.id == id)

  def foodNeeds(kitchen: Kitchen) = kitchen.size * Constants.DAILY_FOOD_NEED_PER_PERSON * 365

  def cultivatedSurface(world: World, kitchen: Kitchen): Double = {
    val cultivatedParcels = World.farmedParcelsForKitchen(world, kitchen).filter(p => Parcel.isCultivated(p))
    cultivatedParcels.map(_.area).sum
  }

  def parcelProduction(parcel: Parcel) = {
    parcel.fertility * (parcel.crop match {
      case Mil => parcel.area * Constants.MIL_YIELD_PER_M2
      case Peanut => parcel.area * Constants.PEANUT_YIELD_PER_M2 * Constants.PEANUT_FOOD_EQUIVALENCE
      case _ => 0.0
    })
  }

  // Fallow is considered as Mil in case of ExtraParcelsExceptFallowLoaner and will be set as Mil once the loan will be effective 
  def parcelProductionForLoan(parcel: Parcel) = {
    parcel.fertility * (parcel.crop match {
      case Mil | Fallow => parcel.area * Constants.MIL_YIELD_PER_M2
      case Peanut => parcel.area * Constants.PEANUT_YIELD_PER_M2 * Constants.PEANUT_FOOD_EQUIVALENCE
      case _ => 0.0
    })
  }

  def parcelsProduction(parcels: Seq[Parcel]) = {
    parcels.map {
      parcelProduction
    }.sum
  }

  def foodBalance(world: World, kitchen: Kitchen): FoodBalance = {
    foodBalance(world.parcels, kitchen)
  }

  def foodBalance(parcels: Seq[Parcel], kitchen: Kitchen): FoodBalance = {
    FoodBalance(kitchen, parcelsProduction(World.parcelsInCultureForKitchen(parcels, kitchen)) - foodNeeds(kitchen))
  }


  def evolve(simulationState: SimulationState, populationGrowth: Double, foodAssessment: Seq[FoodBalance])(using mT: MersenneTwister) = {

    val (populationUpdated, births): (Seq[Kitchen], Map[KitchenID, Int]) = Population.evolve(simulationState.kitchens, populationGrowth)
    val (emigrantsUpdated, nbEmigrants): (Seq[Kitchen], Map[KitchenID, Int]) = Population.evolveEmigrants(populationUpdated, foodAssessment)
    val (afterAbsorbtionKitchens, afterAbsorbtionWorld, absorbingKitchens) = kitchenAbsorption(emigrantsUpdated, simulationState.world)
    val (afterSplitKitchens, afterSplitWorld, splittedInto) = kitchenSplit(afterAbsorbtionKitchens, afterAbsorbtionWorld)

    val populations = afterSplitKitchens.map { k =>
      val birthK = births.getOrElse(k.id, 0)
      val emigrantsK = nbEmigrants.getOrElse(k.id, 0)
      val absorbtionsK = absorbingKitchens.getOrElse(k.id, Seq())
      val splittedIntoK = splittedInto.get(k.id)
      k.id -> History.PopulationStat(k.size, birthK, emigrantsK, k.herdSize, absorbtionsK, splittedIntoK)
    }

    simulationState.copy(
      kitchens = afterSplitKitchens,
      world = afterSplitWorld,
      history =
        simulationState.history
          .updatePopulations(simulationState.year, populations)
          .updateParcelStatsAfterPopulationEvolution(simulationState.year, afterSplitKitchens, afterSplitWorld)
    )

  }

  case class AbsorbingKitchen(kitchen: Kitchen, absorbedIDs: Seq[KitchenID])

  def kitchenAbsorption(kitchens: Seq[Kitchen], world: World)(using mT: MersenneTwister): (Seq[Kitchen], World, Map[KitchenID, Seq[KitchenID]]) = {

    val toBeAbsorbedKitcken = kitchens.filter(k => k.size <= Constants.KITCHEN_MINIMUM_SIZE)
    val absorbingCandidateKitchens = kitchens.diff(toBeAbsorbedKitcken).map { k => AbsorbingKitchen(k, Seq()) }
    // Do not consider too large kitchens to avoid absorbtion/split loops
    val (absorbingKitchens, tooBigKitchens) = absorbingCandidateKitchens.partition(_.kitchen.size <= Constants.KITCHEN_SIZE_THRESHOLD_FOR_ABSORPTION)

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
        absorption(toBeAbsorbed.tail, newAbsorbing)
      }
    }

    val reorganizedKitchens = absorption(toBeAbsorbedKitcken, absorbingKitchens)
    val reorganizedWorld = {
      // get the match between each absorbed id and its absorbing id
      val absorbedMap = reorganizedKitchens.flatMap(k => k.absorbedIDs.map(_ -> k.kitchen.id)).toMap

      world.copy(parcels = world.parcels.map { p =>
        p.copy(ownerID = absorbedMap.getOrElse(p.ownerID, p.ownerID))
      })
    }

    (reorganizedKitchens.map(_.kitchen) ++ tooBigKitchens.map {
      _.kitchen
    }, reorganizedWorld, reorganizedKitchens.map { rK => rK.kitchen.id -> rK.absorbedIDs }.toMap)
  }

  def kitchenSplit(kitchens: Seq[Kitchen], world: World): (Seq[Kitchen], World, Map[KitchenID, KitchenID]) = {
    val toBeSplittedKitchens = kitchens.filter {
      _.size >= (Constants.KITCHEN_MAXIMUM_SIZE - Constants.KITCHEN_MINIMUM_SIZE)
    }

    case class Offspring(kitchen: Kitchen, originKichen: Kitchen, parcels: Seq[Parcel])

    @tailrec
    def split(toBeSplitted: List[Kitchen], highestID: Int, offsprings: List[Offspring]): (Int, List[Offspring]) = {
      if (toBeSplitted.isEmpty) (highestID, offsprings)
      else {
        val kitchenK = toBeSplitted.head
        val parcelsK = World.ownedParcelsForKitchen(world, kitchenK)
        val targetArea = parcelsK.map(_.area).sum * Constants.SPLIT_KITCHEN_OFFSPRING_SIZE / kitchenK.size

        @tailrec
        def acquireParcels(allParcels: List[Parcel], acquiredArea: Double, acquiredParcels: List[Parcel]): List[Parcel] = {
          if (allParcels.isEmpty || acquiredArea > targetArea) acquiredParcels
          else {
            val newParcel = allParcels.head
            acquireParcels(allParcels.tail, acquiredArea + newParcel.area, acquiredParcels :+ newParcel)
          }
        }

        val nextID = highestID + 1
        val acquiredParcelK = acquireParcels(parcelsK.toList.sortBy(_.area), 0.0, List())
        val offspringHerdSize = ((kitchenK.herdSize *  Constants.SPLIT_KITCHEN_OFFSPRING_SIZE).toDouble / kitchenK.size).floor.toInt
        val offspring = Offspring(kitchenK.copy(id = nextID, size = Constants.SPLIT_KITCHEN_OFFSPRING_SIZE, herdSize = offspringHerdSize),
          kitchenK.copy(size = kitchenK.size - Constants.SPLIT_KITCHEN_OFFSPRING_SIZE, herdSize = kitchenK.herdSize - offspringHerdSize),
          acquiredParcelK)

        split(toBeSplitted.tail, nextID, offsprings :+ offspring)
      }
    }

    val (nextHighestID, offsprings) = split(toBeSplittedKitchens.toList, world.highestKitckenID, List())
    val parcelsToBeChangedWithID = offsprings.flatMap { o => o.parcels.map { p => p -> o.kitchen.id } }.toMap
    val parcelsToBeChanged = parcelsToBeChangedWithID.map(_._1).toSeq
    val (newKitchenIDParcels, untouchedParcel) = world.parcels.partition(p => parcelsToBeChanged.contains(p))


    val newWorld = world.copy(parcels = untouchedParcel ++ newKitchenIDParcels.map { p => p.copy(ownerID = parcelsToBeChangedWithID(p), farmerID = parcelsToBeChangedWithID(p)) }, highestKitckenID = nextHighestID)
    val originKitchens = offsprings.map {
      _.originKichen.id
    }
    val newKitchens = kitchens.filterNot(k => originKitchens.contains(k.id)) ++ offsprings.map {
      _.originKichen
    } ++ offsprings.map(_.kitchen)

    (newKitchens, newWorld, offsprings.map { o => o.originKichen.id -> o.kitchen.id }.toMap)
  }

  // Returns parcels in culture if required to satisfied needs of the kitchen and not assigned parcels if not
  def cropNeeds(kitchen: Kitchen, cultivableParcelsForKitchen: Seq[Parcel], needs: Option[Double]) = {

    val sortedByDistanceParcels = cultivableParcelsForKitchen.sortBy(_.distanceToVillage).toList
    val manPower = Kitchen.manPower(kitchen)

    // println("NEEDS " + needs + " MAN POVER " + manPower + " for " + kitchen.id + " with  " + kitchen.size + " people")

    @tailrec
    def cropsToBeCultivated(kitchen: Kitchen, production: Double, sortedParcels: List[Parcel], inCulture: List[Parcel], remainingManPower: Double): Seq[Parcel] = {

      val needsCondition = needs match {
        case Some(n) => production > n
        case _ => false
      }

      // println("CROPNEEDS " + sortedParcels.length + " // " + needs + " / " + production + " // " + remainingManPower)
      if (sortedParcels.isEmpty || needsCondition || remainingManPower < 0) {

        // println("TOTAL PRODUCTION for " + kitchen.id + " : " + production + " for " + kitchen.size + " people, soient " + inCulture.map{_.area}.sum + " m2" + ", remaining man power: " + remainingManPower)
        inCulture //++: sortedParcels.map { p => p.copy(crop = NotAssigned) }

      }
      else {
        val parcel = sortedParcels.head
        val pproduction = Kitchen.parcelProduction(parcel)
        cropsToBeCultivated(kitchen, production + pproduction, sortedParcels.tail, inCulture :+ parcel, remainingManPower - manPowerFor(parcel.area))
      }
    }

    cropsToBeCultivated(kitchen, 0.0, sortedByDistanceParcels, List(), manPower)
  }

  // The maximum area of crops in culture regarding to the available manpower
  def areaOfPossibleCropsInCulture(kitchen: Kitchen) = {
    manPower(kitchen).ceil * Constants.CULTIVATED_AREA_PER_WORKER // ceil because kids and teenagers can help occasionally
  }

  def manPower(kitchen: Kitchen) = (kitchen.size * Constants.WORKERS_RATIO_PER_KITCHEN).ceil

  def manPowerFor(cultureArea: Double) = cultureArea / Constants.CULTIVATED_AREA_PER_WORKER
}

case class Kitchen(id: Kitchen.KitchenID,
                   size: Int,
                   rotationCycle: RotationCycle,
                   cropingStrategy: CropingStrategy,
                   loanStrategy: LoanStrategy,
                   foodDonationStrategy: FoodDonationStrategy,
                   herdSize: Int,
                   drySeasonHerdStrategy: HerdStrategy,
                   wetSeasonHerdStrategy: HerdStrategy,
                   fertilizerStrategy: FertilizerStrategy
                  )