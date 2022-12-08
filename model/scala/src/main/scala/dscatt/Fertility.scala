package dscatt

import dscatt.Croping.Crop
import dscatt.Croping.*
import dscatt.Constants.*
import dscatt.History.History
import dscatt.Parcel.{ManureDeposit, ParcelID}
import dscatt.Simulation.SimulationState

import scala.annotation.tailrec

object Fertility {

  //TODO
  // 1) depot de manure sur toutes les parcelles selon la strategie: Map[ParcelID, ManureDeposit]
  // 2) Mettre a jour les manureDeposits et qS de toutes les parcelles du monde: World
  // 3) Calcul des biomassProduction pour chaque parcelle (N * parcelArea * cropConstantConversion): Seq[FoodBalance]

  type FertilityBoost = Double

  def dungMassFor(parcel: Parcel, herdSize: Int, toBeAmendedArea: Double): Double = KG_OF_MANURE_PER_COW_PER_YEAR * herdSize * parcel.area / toBeAmendedArea

  def uniformFertilizerEffect(fertilizerWeight: Int): FertilityBoost = FERTILITY_BOOST_PER_FERTILIZER_KG * fertilizerWeight

  def cropFertilityBoost(crop: Crop): FertilityBoost = {
    crop match {
      case Mil => MIL_EFFECT_ON_FERTILITY_BOOST
      case Peanut => PEANUT_EFFECT_ON_FERTILITY_BOOST
      case _ => FALLOW_EFFECT_FERTILITY_BOOST // the parcel is resting whatever it is NotAssigned or Fallow
    }
  }

  case class DryKitchenManuringProcess(herdStrategy: HerdStrategy, toBeManuredParcels: Seq[Parcel], otherParcels: Seq[Parcel], herdSize: Int)

  //  def manure(kitchenManuringProcesse: KitchenManuringProcess, allParcelsArea: Double, allFallowParcelsArea: Double): Double = (hStrategy.dry match {
  //    case EverywhereByDayOwnerByNight =>
  //      0.83 * (0.2 * uniformManureEffect(allParcelsArea, hStrategy.herdSize) /* day */ + 0.8 * uniformManureEffect(hStrategy.kitchenParcelsArea, hStrategy.herdSize) /* night */) + // dry season
  //        0.17 * (0.2 * uniformManureEffect(allFallowParcelsArea, hStrategy.herdSize /* day */) + 0.8 * uniformManureEffect(hStrategy.kitchenFallowParcelsArea, hStrategy.herdSize) /* night */) // wet season
  //    case EverywhereAllTime =>
  //      0.83 * uniformManureEffect(allParcelsArea, hStrategy.herdSize) + 0.17 * uniformManureEffect(allFallowParcelsArea, hStrategy.herdSize)
  //    case OwnerOnly =>
  //      0.83 * uniformManureEffect(hStrategy.kitchenParcelsArea, hStrategy.herdSize) + 0.17 * uniformManureEffect(hStrategy.kitchenFallowParcelsArea, hStrategy.herdSize)
  //  }
  //    )

  def assign(state: SimulationState): SimulationState = {

    val dryManuringProcesses = state.kitchens.map { k =>
      val farmedParcels = World.farmedParcelsForKitchen(state.world, k)
      val (dryToBeManured, dryOthers) = farmedParcels.partition(p => k.drySeasonManureCriteria(p, k.rotationCycle))

      DryKitchenManuringProcess(k.drySeasonHerdStrategy, dryToBeManured, dryOthers, k.herdSize)
    }

    val totalDungMass = state.kitchens.map(_.herdSize * KG_OF_MANURE_PER_COW_PER_YEAR).sum

    val fullArea = World.fullArea(state.world)
    val allFallowArea = World.fallowParcels(state.world).map(_.area).sum


    def herdVillageContributionToParcelManureMass(parcel: Parcel) = parcel.area / fullArea * totalDungMass

    def herdVillageContributionToFallowParcelMass(parcel: Parcel) = parcel.area / allFallowArea * totalDungMass

    @tailrec
    def dryDeposeManure(kitchenManuringProcesses: List[DryKitchenManuringProcess], deposits: List[(Parcel, ManureDeposit)]): Seq[(Parcel, ManureDeposit)] = {
      if (kitchenManuringProcesses.isEmpty) deposits
      else {
        val currentProcess = kitchenManuringProcesses.head
        val manuredAreaK = currentProcess.toBeManuredParcels.map(_.area).sum
        val newDepositsByParcel = currentProcess.herdStrategy match {
          case EverywhereByDayOwnerByNight =>
            currentProcess.toBeManuredParcels.map { p =>
              val dungMassForP = dungMassFor(p, currentProcess.herdSize, manuredAreaK)
              val deposit = (0.8 * dungMassForP) + (0.2 * herdVillageContributionToParcelManureMass(p))
              p -> ManureDeposit(state.year, deposit)
              // p.copy(manureDeposits = p.manureDeposits :+ ManureDeposit(state.year, deposit))
            } ++ currentProcess.otherParcels.map(p => p -> ManureDeposit(state.year, 0.2 * herdVillageContributionToParcelManureMass(p)))
          case AnywhereAnyTime =>
            (currentProcess.toBeManuredParcels ++ currentProcess.otherParcels).map { p =>
              p -> ManureDeposit(state.year, herdVillageContributionToParcelManureMass(p))
            }
          case OwnerOnly => currentProcess.toBeManuredParcels.map { p =>
            val deposit = dungMassFor(p, currentProcess.herdSize, manuredAreaK)
            p -> ManureDeposit(state.year, deposit)
          }
        }
        dryDeposeManure(kitchenManuringProcesses.tail, deposits ++ newDepositsByParcel)
      }
    }

    @tailrec
    def wetDeposeManure(kitchens: List[Kitchen], deposits: List[(Parcel, ManureDeposit)]): Seq[(Parcel, ManureDeposit)] = {
      if (kitchens.isEmpty) deposits
      else {
        val k = kitchens.head
        val fallows = World.farmedParcelsForKitchen(state.world, k)
        val totalFallowAreaK = fallows.map(_.area).sum
        val newManureDepositByParcel = k.wetSeasonHerdStrategy match {
          case EverywhereByDayOwnerByNight =>
            fallows.map { p =>
              val deposit = 0.8 * dungMassFor(p, k.herdSize, totalFallowAreaK) + 0.2 * herdVillageContributionToFallowParcelMass(p)
              p -> ManureDeposit(state.year, deposit)
            }
          case AnywhereAnyTime => fallows.map { p =>
            p -> ManureDeposit(state.year, herdVillageContributionToFallowParcelMass(p))
          }
          case OwnerOnly => fallows.map { p =>
            p -> ManureDeposit(state.year, dungMassFor(p, k.herdSize, totalFallowAreaK))
          }
        }
        wetDeposeManure(kitchens.tail, deposits ++ newManureDepositByParcel)
      }
    }

    val newParcels = (dryDeposeManure(dryManuringProcesses.toList, List()) ++ wetDeposeManure(state.kitchens.toList, List())).groupBy {
      _._1.id
    }.values.map { ps =>
      ps.reduce { case (dry: (Parcel, ManureDeposit), wet: (Parcel, ManureDeposit)) =>
        dry._1.copy(manureDeposits = dry._1.manureDeposits :+ ManureDeposit(dry._2.year, 0.7 * dry._2.quantity + 0.3 * wet._2.quantity)) -> ManureDeposit(0, 0.0) // 0.7 is the dry season contribution, 0.3 the wet one
      }
    }.map(_._1).toSeq

    val newWorld = state.world.copy(parcels = newParcels)
    state.copy(world = newWorld, history = state.history.updateFertilitySats(state.year, newWorld, state.kitchens))
  }


  //    val manureFertilityBoost = kitchenManuringProcesses.map { case (id, hs) =>
  //      id -> manure(hs, allParcelsArea, allFallowParcelsArea)
  //    }.toMap
  //
  //    println("MATURE fertilities boost " + manureFertilityBoost)
  //    val updatedFertilityParcels = state.world.parcels.map { p =>
  //      p.copy(soilQuality = p.soilQuality * (1.0 +
  //        cropFertilityBoost(p.crop) +
  //        manureFertilityBoost(p.ownerID)))
  //    }
  //
  //    val newWorld = state.world.copy(parcels = updatedFertilityParcels)
  //    state.copy(world = newWorld, history = state.history.updateFertilitySats(state.year, state.world, state.kitchens))

  def biomassProduction(parcel: Parcel, history: History, year: Int) = {

    //FIXME: 60/40 manureQuantityLastYear, tout ça
    val depositNitrogen = 1.0

    val availableNitrogen = soilNitrogen(parcel) + airNitrogen(parcel) + depositNitrogen + faidherbiaNitrogen(parcel)

    parcel.crop match {
      case Mil => milNRF(availableNitrogen)
      case Fallow | NotAssigned => fallowNRF(availableNitrogen)
      case _ => PEANUT_NRF
    }
  }

  def soilNitrogen(parcel: Parcel) = {
    0.0012 * parcel.area * qs(parcel)
  }

  def airNitrogen(parcel: Parcel) = parcel.area * 0.002

  def faidherbiaNitrogen(parcel: Parcel) = {
    0.0
    // parcel.fedherbiaTrees * 4
  }

  def qs(parcel: Parcel) = {
    1.0
  }

  def manureDeltaQS(herdSize: Int) = {
    1.0
  }

  def milNRF(nAvailable: Double) = nAvailable match {
    case n if n < 18 => 0.25
    case n if n >= 18 && n <= 83 => 0.501 * Math.log(nAvailable) - 1.2179
    case _ => 1.0
  }

  def fallowNRF(nAvailable: Double) = nAvailable match {
    case n if n < 10 => 0.25
    case n if n >= 10 && n <= 50 => 0.501 * Math.log(nAvailable) - 1.2179
    case _ => 1.0
  }
}
