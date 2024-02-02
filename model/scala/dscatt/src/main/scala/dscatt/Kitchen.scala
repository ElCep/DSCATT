package dscatt

import Croping.*
import Fertility.{AgronomicMetricsByParcel, milSeedFullPontential, peanutSeedFullPotential}
import Simulation.SimulationState
import dscatt.Constants.NB_BY_HA
import org.apache.commons.math3.random.MersenneTwister
import Constants.*

import scala.annotation.tailrec

object Kitchen {

  type KitchenID = Int

  case class FoodBalance(kitchenID: KitchenID, balance: Double)

  case class Food(kitchenID: KitchenID, needs: Double = 0.0, fromCulture: Double = 0.0, fromLoan: Double = 0.0, fromDonation: Double = 0.0, inexess: Double = 0.0, fromMil: Double = 0.0, milInCultureArea: Double = 0.0, fromPeanut: Double = 0.0, peanutInCultureArea: Double = 0.0)

  implicit class WrapFood(f: Food):
    def toBalance = f.needs + f.fullProduction

    def fullProduction = f.fromCulture + f.fromLoan + f.fromDonation

  def buildKitchens(kitchenPartition: KitchenPartition): Seq[Kitchen] = {
    kitchenPartition.profiles.flatMap { p => Seq.fill[KitchenProfile](p._2)(p._1) }.zipWithIndex.map { case (kp, id) =>
      Kitchen(
        id + 1,
        kp.kitchenSize,
        kp.rotationCycle,
        kp.cropingStrategy,
        kp.ownFallowUse,
        kp.loanStrategy,
        kp.foodDonationStrategy,
        kp.drySeasonHerdStrategy,
        kp.wetSeasonHerdStrategy,
        kp.drySeasonManureCriteria,
        kp.fertilizerStrategy,
        kp.mulchingStrategy,
        kp.nbFaidherbia
      )
    }
  }

  def kitchen(kitchens: Seq[Kitchen], id: KitchenID) = kitchens.find(_.id == id)

  def foodNeeds(kitchen: Kitchen) = kitchen.size * Constants.DAILY_FOOD_NEED_PER_PERSON * 365

  def parcelFoodProduction(parcel: Parcel)(using metrics: Fertility.AgronomicMetricsByParcel, rainFall: MM): Double = {
    parcelFoodProduction(parcel.crop, parcel.area, metrics(parcel.id), rainFall)
  }


  def parcelFoodProduction(crop: Crop, parcelArea: Double, agronomicMetrics: Fertility.AgronomicMetrics, rainFall: MM): Double = {
    (crop match {
      case Mil => Fertility.milNRF(agronomicMetrics, parcelArea) * milSeedFullPontential(rainFall)
      case Peanut => Fertility.peanutNRF * peanutSeedFullPotential * Constants.PEANUT_FOOD_EQUIVALENCE
      case _ => 0.0
    }) * parcelArea
  }

  // Fallow and Peanut are considered as Mil in case of loan (since loan is for food) and will be set as Mil once the loan will be effective
  def parcelFoodProductionForLoan(parcel: Parcel)(using metrics: Fertility.AgronomicMetricsByParcel, rainFall: MM) =
    (parcel.crop match {
      case Mil | Fallow | Peanut => Fertility.milNRF(metrics(parcel.id), parcel.area) * milSeedFullPontential(rainFall)
    }) * parcel.area


  def parcelsFoodProduction(parcels: Seq[Parcel])(using metrics: Fertility.AgronomicMetricsByParcel, rainFall: MM) = {
    parcels.map {
      parcelFoodProduction
    }.sum
  }

  def foodBalance(world: World, kitchen: Kitchen)(using metrics: Fertility.AgronomicMetricsByParcel, rainFall: MM): FoodBalance = {
    foodBalance(world.parcels, kitchen)
  }

  def foodBalance(parcels: Seq[Parcel], kitchen: Kitchen)(using Fertility.AgronomicMetricsByParcel, MM): FoodBalance = {
    FoodBalance(kitchen.id, parcelsFoodProduction(World.parcelsInCultureForKitchen(parcels, kitchen)) - foodNeeds(kitchen))
  }


  def evolve(
              simulationState: SimulationState,
              populationGrowth: Double,
              foodAssessment: Seq[Food],
              emigrationProcess: Boolean = true,
            )(using mT: MersenneTwister) = {

    val (populationUpdated, births): (Seq[Kitchen], Map[KitchenID, Int]) = Population.evolve(simulationState.kitchens, populationGrowth)

    val (emigrantsUpdated, nbEmigrants): (Seq[Kitchen], Map[KitchenID, Int]) =
      emigrationProcess match {
        case true => Population.evolveEmigrants(populationUpdated, foodAssessment)
        case false => (populationUpdated, Map())
      }

    val (afterSplitKitchens, afterSplitWorld, splittedInto) = kitchenSplit(emigrantsUpdated, simulationState.world)
    val (afterAbsorbtionKitchens, afterAbsorbtionWorld, absorbingKitchens) = kitchenAbsorption(afterSplitKitchens, afterSplitWorld)

    val populations = afterAbsorbtionKitchens.map { k =>
      val birthK = births.getOrElse(k.id, 0)
      val emigrantsK = nbEmigrants.getOrElse(k.id, 0)
      val absorbtionsK = absorbingKitchens.getOrElse(k.id, Seq())
      val splittedIntoK = splittedInto.get(k.id)
      k.id -> History.PopulationStat(k.size, birthK, emigrantsK, absorbtionsK, splittedIntoK)
    }

    simulationState.copy(
      kitchens = afterAbsorbtionKitchens,
      world = afterAbsorbtionWorld,
      history =
        simulationState.history
          .updatePopulations(simulationState.year, populations)
          .updateParcelStatsAfterPopulationEvolution(simulationState.year, afterAbsorbtionKitchens, afterAbsorbtionWorld)
    )

  }

  case class AbsorbingKitchen(kitchen: Kitchen, absorbedIDs: Seq[KitchenID])

  def kitchenAbsorption(
                         kitchens: Seq[Kitchen],
                         world: World
                       )(using mT: MersenneTwister): (Seq[Kitchen], World, Map[KitchenID, Seq[KitchenID]]) = {

    val kitchenSizeThresholdForAbsorption = Constants.KITCHEN_MAXIMUM_SIZE
    val toBeAbsorbedKitcken = kitchens.filter(k => k.size <= Constants.KITCHEN_MINIMUM_SIZE)
    val absorbingCandidateKitchens = kitchens.diff(toBeAbsorbedKitcken).map { k => AbsorbingKitchen(k, Seq()) }
    // Do not consider too large kitchens to avoid absorbtion/split loops
    val (absorbingKitchens, tooBigKitchens) = absorbingCandidateKitchens.partition(_.kitchen.size <= kitchenSizeThresholdForAbsorption)

    @tailrec
    def absorption(toBeAbsorbed: Seq[Kitchen], absorbing: Seq[AbsorbingKitchen]): Seq[AbsorbingKitchen] = {
      val nbAbsorbing = absorbing.length
      if (toBeAbsorbed.length == 0 || nbAbsorbing < 1) {
        absorbing
      }
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

  def kitchenSplit(kitchens: Seq[Kitchen], world: World):
  (Seq[Kitchen], World, Map[KitchenID, KitchenID]) = {
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
        val offspring = Offspring(kitchenK.copy(id = nextID, size = Constants.SPLIT_KITCHEN_OFFSPRING_SIZE),
          kitchenK.copy(size = kitchenK.size - Constants.SPLIT_KITCHEN_OFFSPRING_SIZE),
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

  case class CropNeeded(cultivatedParcels: Seq[Parcel], candidatesNotUsed: Seq[Parcel], inexcessOnCultivatedParcels: Double)

  // Returns parcels in culture if required to satisfied needs of the kitchen and not assigned parcels if not
  // If fallows are present in the cultivableParcelForKitchen, it means the fallow can be used as a culture. In that case it is switched to a mil
  def getCropNeeded(kitchen: Kitchen, cultivableParcelsForKitchen: Seq[Parcel], needs: Double)(using Fertility.AgronomicMetricsByParcel, MM) = {

    val manPower = Kitchen.manPower(kitchen)

    @tailrec
    def cropsToBeCultivated(kitchen: Kitchen, production: Double, sortedParcels: List[Parcel], inCulture: List[Parcel], remainingManPower: Double): CropNeeded = {

      val needsCondition = production > needs

      if (sortedParcels.isEmpty || needsCondition || remainingManPower < 0)
        CropNeeded(inCulture, sortedParcels, if (needsCondition) production - needs else 0.0)
      else {
        val head = sortedParcels.head

        val parcel = head.crop match {
          case Fallow => head.copy(crop = Mil)
          case _ => head
        }

        val pproduction = Kitchen.parcelFoodProduction(parcel)
        cropsToBeCultivated(kitchen, production + pproduction, sortedParcels.tail, inCulture :+ parcel, remainingManPower - manPowerFor(parcel.area))
      }
    }

    cropsToBeCultivated(kitchen, 0.0, cultivableParcelsForKitchen.toList, List(), manPower)
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
                   ownFallowUse: OwnFallowUse,
                   loanStrategy: LoanStrategy,
                   foodDonationStrategy: FoodDonationStrategy,
                   drySeasonHerdStrategy: HerdStrategy,
                   wetSeasonHerdStrategy: HerdStrategy,
                   drySeasonManureCriteria: (Parcel, RotationCycle) => Boolean, //how to choose parcel to be fertilized during dry season
                   fertilizerStrategy: FertilizerStrategy,
                   mulchingStrategy: MulchingStrategy,
                   nbFaidherbia: NB_BY_HA
                  )