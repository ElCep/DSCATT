package dscatt

import Croping.*
import Fertility.{milSeedFullPontential, peanutSeedFullPotential}
import Simulation.SimulationState
import Data.*
import org.apache.commons.math3.random.MersenneTwister

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

  def foodNeeds(kitchen: Kitchen, data: Data) = kitchen.size * data.DAILY_FOOD_NEED_PER_PERSON * 365

  def parcelFoodProduction(parcel: Parcel, data: Data, year: Int): Double = {
    (parcel.crop match {
      case Millet => Fertility.milNRF(parcel, data, year) * milSeedFullPontential(data)
      case Peanut => Fertility.peanutNRF * peanutSeedFullPotential(data) * data.PEANUT_FOOD_EQUIVALENCE
      case _ => 0.0
    }) * parcel.area
  }

  // Fallow and Peanut are considered as Mil in case of loan (since loan is for food) and will be set as Mil once the loan will be effective
  def parcelFoodProductionForLoan(parcel: Parcel, data: Data, year: Int) =
    (parcel.crop match {
      case Millet | Fallow | Peanut => Fertility.milNRF(parcel, data, year) * milSeedFullPontential(data)
    }) * parcel.area


  def parcelsFoodProduction(parcels: Seq[Parcel], data: Data, year: Int) = {
    parcels.map { p =>
      parcelFoodProduction(p, data, year)
    }.sum
  }

  def foodBalance(world: World, kitchen: Kitchen, data: Data, year: Int): FoodBalance = {
    foodBalance(world.parcels, kitchen, data, year)
  }

  def foodBalance(parcels: Seq[Parcel], kitchen: Kitchen, data: Data, year: Int): FoodBalance = {
    FoodBalance(kitchen.id, parcelsFoodProduction(World.parcelsInCultureForKitchen(parcels, kitchen), data, year) - foodNeeds(kitchen, data))
  }

  def evolve(
              simulationState: SimulationState,
              populationGrowth: Double,
              foodAssessment: Seq[Food],
              emigrationProcess: Boolean = true,
              data: Data
            )(using mT: MersenneTwister) = {

    val (populationUpdated, births): (Seq[Kitchen], Map[KitchenID, Int]) = Population.evolve(simulationState.kitchens, populationGrowth)

    val (emigrantsUpdated, nbEmigrants): (Seq[Kitchen], Map[KitchenID, Int]) =
      emigrationProcess match {
        case true => Population.evolveEmigrants(populationUpdated, foodAssessment, data)
        case false => (populationUpdated, Map())
      }

    val (afterSplitKitchens, afterSplitWorld, splittedInto) = kitchenSplit(emigrantsUpdated, simulationState.world, data)
    val (afterAbsorbtionKitchens, afterAbsorbtionWorld, absorbingKitchens) = kitchenAbsorption(afterSplitKitchens, afterSplitWorld, data)

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
                         world: World,
                         data: Data
                       ): (Seq[Kitchen], World, Map[KitchenID, Seq[KitchenID]]) = {

    val kitchenSizeThresholdForAbsorption = data.KITCHEN_MAXIMUM_SIZE
    val toBeAbsorbedKitcken = kitchens.filter(k => k.size <= data.KITCHEN_MINIMUM_SIZE)
    val absorbingCandidateKitchens = kitchens.diff(toBeAbsorbedKitcken).map { k => AbsorbingKitchen(k, Seq()) }
    // Do not consider too large kitchens to avoid absorbtion/split loops
    val (absorbingKitchens, tooBigKitchens) = absorbingCandidateKitchens.partition(_.kitchen.size <= kitchenSizeThresholdForAbsorption)

    @tailrec
    def absorption(toBeAbsorbed: Seq[Kitchen], absorbing: Seq[AbsorbingKitchen], alreadyAbsorbing: Seq[AbsorbingKitchen]): (Seq[AbsorbingKitchen], Seq[Kitchen]) = {
      if (toBeAbsorbed.length == 0 || absorbing.length < 1)
        (alreadyAbsorbing, toBeAbsorbed ++ absorbing.map(_.kitchen))
      else {
        val oneAbsorbing = absorbing.head
        val newAbsorbing = oneAbsorbing.copy(
          kitchen = oneAbsorbing.kitchen.copy(size = oneAbsorbing.kitchen.size + toBeAbsorbed.head.size),
          absorbedIDs = oneAbsorbing.absorbedIDs :+ toBeAbsorbed.head.id
        )

        absorption(toBeAbsorbed.tail, absorbing.tail, alreadyAbsorbing :+ newAbsorbing)
      }
    }

    val (reorganizedKitchens, untouchedKitchens) = absorption(toBeAbsorbedKitcken, absorbingKitchens, Seq())

    val reorganizedWorld = {
      // get the match between each absorbed id and its absorbing id
      val absorbedMap = reorganizedKitchens.flatMap(k => k.absorbedIDs.map(_ -> k.kitchen.id)).toMap

      world.copy(parcels = world.parcels.map { p =>
        p.copy(ownerID = absorbedMap.getOrElse(p.ownerID, p.ownerID))
      })
    }

    (untouchedKitchens ++ reorganizedKitchens.map(_.kitchen) ++ tooBigKitchens.map {
      _.kitchen
    }, reorganizedWorld, reorganizedKitchens.map { rK => rK.kitchen.id -> rK.absorbedIDs }.toMap)
  }

  def kitchenSplit(kitchens: Seq[Kitchen], world: World, data: Data):
  (Seq[Kitchen], World, Map[KitchenID, KitchenID]) = {
    val toBeSplittedKitchens = kitchens.filter {
      _.size >= (data.KITCHEN_MAXIMUM_SIZE - data.KITCHEN_MINIMUM_SIZE)
    }

    case class Offspring(kitchen: Kitchen, originKichen: Kitchen, parcels: Seq[Parcel])

    @tailrec
    def split(toBeSplitted: List[Kitchen], highestID: Int, offsprings: List[Offspring]): (Int, List[Offspring]) = {
      if (toBeSplitted.isEmpty) (highestID, offsprings)
      else {
        val kitchenK = toBeSplitted.head
        val parcelsK = World.ownedParcelsForKitchen(world, kitchenK)
        val targetArea = parcelsK.map(_.area).sum * data.SPLIT_KITCHEN_OFFSPRING_SIZE / kitchenK.size

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
        val offspring = Offspring(kitchenK.copy(id = nextID, size = data.SPLIT_KITCHEN_OFFSPRING_SIZE),
          kitchenK.copy(size = kitchenK.size - data.SPLIT_KITCHEN_OFFSPRING_SIZE),
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
  def getCropNeeded(kitchen: Kitchen, cultivableParcelsForKitchen: Seq[Parcel], needs: Double, data: Data, year: Int) = {

    @tailrec
    def cropsToBeCultivated(kitchen: Kitchen, production: Double, sortedParcels: List[Parcel], inCulture: List[Parcel]): CropNeeded = {

      val needsCondition = production > needs

      if (sortedParcels.isEmpty || needsCondition)
        CropNeeded(inCulture, sortedParcels, if (needsCondition) production - needs else 0.0)
      else {
        val head = sortedParcels.head

        val parcel = head.crop match {
          case Fallow => head.copy(crop = Millet)
          case _ => head
        }

        val pproduction = Kitchen.parcelFoodProduction(parcel, data, year)
        cropsToBeCultivated(kitchen, production + pproduction, sortedParcels.tail, inCulture :+ parcel)
      }
    }

    cropsToBeCultivated(kitchen, 0.0, cultivableParcelsForKitchen.toList, List())
  }

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