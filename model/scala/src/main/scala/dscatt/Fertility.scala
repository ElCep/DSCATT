package dscatt

import dscatt.Croping.Crop
import dscatt.Croping.*
import dscatt.Constants.*
import dscatt.History.History
import dscatt.Parcel.{ManureDeposit, ParcelID}
import dscatt.Simulation.SimulationState

import scala.annotation.tailrec

object Fertility {

  type FertilityBoost = Double

  case class AgronomicMetrics(availableNitrogen: Double = 0.0, soilQuality: Double = 0.0)

  case class Metrics(year: Int, manureMass: Double = 0.0, mulchingMass: Double = 0.0, agronomicMetrics: AgronomicMetrics = AgronomicMetrics(0.0, 0.0))

  type AgronomicMetricsByParcel = Map[ParcelID, AgronomicMetrics]

  def dungMassFor(parcel: Parcel, herdSize: Int, toBeAmendedArea: Double): Double = KG_OF_MANURE_PER_COW_PER_YEAR * herdSize * parcel.area / toBeAmendedArea

  //def uniformFertilizerEffect(fertilizerWeight: Int): FertilityBoost = FERTILITY_BOOST_PER_FERTILIZER_KG * fertilizerWeight

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

  def assign(state: SimulationState, soilQualityBasis: Double): SimulationState = {

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
    def dryDepositManure(kitchenManuringProcesses: List[DryKitchenManuringProcess], deposits: List[(Parcel, Double)]): Seq[(Parcel, Double)] = {
      if (kitchenManuringProcesses.isEmpty) deposits
      else {
        val currentProcess = kitchenManuringProcesses.head
        val manuredAreaK = currentProcess.toBeManuredParcels.map(_.area).sum
        val newDepositsByParcel = currentProcess.herdStrategy match {
          case EverywhereByDayOwnerByNight =>
            currentProcess.toBeManuredParcels.map { p =>
              val dungMassForP = dungMassFor(p, currentProcess.herdSize, manuredAreaK)
              val deposit = (0.8 * dungMassForP) + (0.2 * herdVillageContributionToParcelManureMass(p))
              p -> deposit
            } ++ currentProcess.otherParcels.map(p => p -> 0.2 * herdVillageContributionToParcelManureMass(p))
          case AnywhereAnyTime =>
            (currentProcess.toBeManuredParcels ++ currentProcess.otherParcels).map { p =>
              p -> herdVillageContributionToParcelManureMass(p)
            }
          case OwnerOnly => currentProcess.toBeManuredParcels.map { p =>
            val deposit = dungMassFor(p, currentProcess.herdSize, manuredAreaK)
            p -> deposit
          }
        }
        dryDepositManure(kitchenManuringProcesses.tail, deposits ++ newDepositsByParcel)
      }
    }

    @tailrec
    def wetDepositManure(kitchens: List[Kitchen], deposits: List[(Parcel, Double)]): Seq[(Parcel, Double)] = {
      if (kitchens.isEmpty) deposits
      else {
        val k = kitchens.head
        val fallows = World.farmedParcelsForKitchen(state.world, k)
        val totalFallowAreaK = fallows.map(_.area).sum
        val newManureDepositByParcel = k.wetSeasonHerdStrategy match {
          case EverywhereByDayOwnerByNight =>
            fallows.map { p =>
              val deposit = 0.8 * dungMassFor(p, k.herdSize, totalFallowAreaK) + 0.2 * herdVillageContributionToFallowParcelMass(p)
              p -> deposit
            }
          case AnywhereAnyTime => fallows.map { p =>
            p -> herdVillageContributionToFallowParcelMass(p)
          }
          case OwnerOnly => fallows.map { p =>
            p -> dungMassFor(p, k.herdSize, totalFallowAreaK)
          }
        }
        wetDepositManure(kitchens.tail, deposits ++ newManureDepositByParcel)
      }
    }


    val newParcels = {
      (dryDepositManure(dryManuringProcesses.toList, List()) ++ wetDepositManure(state.kitchens.toList, List())).groupBy {
        _._1.id
      }.values.map { ps =>
        val parcel = ps.head._1
        val manureMass = ps.map {
          _._2
        }.reduce((dry, wet) => dry * 0.7 + wet * 0.3)
        //FIXME Set mulching mass after mil yield
        val mulchingMass = 0.0
        val currentYearSoilQuality = Fertility.soilQuality(parcel, soilQualityBasis)
        val metrics = Fertility.Metrics(state.year, manureMass, mulchingMass, AgronomicMetrics(availableNitrogen(parcel, currentYearSoilQuality), currentYearSoilQuality))
        parcel.copy(fertilityHistory = parcel.fertilityHistory :+ metrics) // 0.7 is the dry season contribution, 0.3 the wet one
      }.toSeq
    }

    val newWorld = state.world.copy(parcels = newParcels)
    state.copy(world = newWorld, history = state.history.updateFertilitySats(state.year, newWorld, state.kitchens))
  }

  private def soilQuality(parcel: Parcel, soilQualityBasis: Double) = {

    val manureBoost =
      // Last 2 years of manure are used to compute the boost: 0.6 for the most recent deposit and 0.4 for the oldest
      val last2Years = parcel.fertilityHistory.takeRight(2)
      last2Years.lastOption.map(_.manureMass * 0.00012).getOrElse(0.0) + last2Years.headOption.map(_.manureMass * 0.00008).getOrElse(0.0)

    val mulchingBoost = parcel.fertilityHistory.lastOption.map(_.mulchingMass).getOrElse(0.0) * 0.001

    // The soil quality it computed at the begining of the year before the the rotation process, so that the parcel crop here
    // is equivalent to the crop of the previous year
    val fallowBoost = parcel.crop match {
      case Croping.Fallow => 0.01
      case _ => 0.0
    }

    val faidherbiaBoost = parcel.faidherbiaTrees * 0.06

    soilQualityBasis + manureBoost + mulchingBoost + fallowBoost + faidherbiaBoost
  }

  // in kg. Computed from previous year soil quality and manure production
  private def availableNitrogen(parcel: Parcel, qs: Double) = {

    val airNitrogen = Constants.ATMOSPHERIC_NITROGEN * parcel.area

    val soilNitrogen = Constants.NITROGEN_MINERALIZATION * parcel.area * qs

    val manureNitrogen =
      val last2Years = parcel.fertilityHistory.takeRight(2)
      Constants.NITROGEN_PROPORTION_PER_MANURE_KG * (last2Years.lastOption.map {
        _.manureMass * 0.6
      }.getOrElse(0.0) + last2Years.headOption.map {
        _.manureMass * 0.4
      }.getOrElse(0.0))

    val faidherbiaNitrogen = 4 * parcel.faidherbiaTrees

    airNitrogen + soilNitrogen + manureNitrogen + faidherbiaNitrogen

  }

  def agronomicMetrics(parcel: Parcel, soilQualityBasis: Double) = {
    val qs = soilQuality(parcel, soilQualityBasis)
    Fertility.AgronomicMetrics(availableNitrogen(parcel, qs), qs)
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

  def peanutNRF = 1.0
}
