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

  case class Metrics(year: Int, crop: Crop = NotAssigned, manureMass: Double = 0.0, mulchingMass: Double = 0.0, agronomicMetrics: AgronomicMetrics = AgronomicMetrics(0.0, 0.0))

  type AgronomicMetricsByParcel = Map[ParcelID, AgronomicMetrics]
  
  def assign(state: SimulationState, soilQualityBasis: Double): SimulationState = {

    def manureVillageForPFor(parcel: Parcel, parcels: Seq[Parcel], state: SimulationState) =
      state.kitchens.map {
        _.herdSize
      }.sum * Constants.KG_OF_MANURE_PER_COW_PER_YEAR / parcels.map {
        _.area
      }.sum * parcel.area

    
    // A parcel is divided into 4 periods of time. First dry (0.7 of the year) and wet (0.3 of the year) season and then
    // night (0.8 of the dung production) and day (0.2 of the dung production).
    @tailrec
    def fertilize(allParcels: Seq[Parcel], fertilityUpdated: Seq[Parcel]): Seq[Parcel] = {
      if (allParcels.isEmpty) fertilityUpdated
      else {
        val parcel = allParcels.head
        val manureMass =
          Kitchen.kitchen(state.kitchens, parcel.farmerID).map { kitchen =>

            val dryToBeManuredArea = World.assignedParcelsForKitchen(state.world, kitchen).filter(p=> kitchen.drySeasonManureCriteria(p, kitchen.rotationCycle)).map {
              _.area
            }.sum

            val manureKforAssignedP = kitchen.herdSize * Constants.KG_OF_MANURE_PER_COW_PER_YEAR * parcel.area / dryToBeManuredArea
            val dryManureVillageForP = manureVillageForPFor(parcel, World.assignedParcels(state.world).filter(p=> kitchen.drySeasonManureCriteria(p, kitchen.rotationCycle)), state)

            val dryMass = Parcel.isAssigned(parcel) match {
              case true =>
                kitchen.drySeasonManureCriteria(parcel, kitchen.rotationCycle) match {
                  case true =>
                    kitchen.drySeasonHerdStrategy match {
                      case EverywhereByDayOwnerByNight => 0.56 * manureKforAssignedP + 0.14 * dryManureVillageForP // 0.7 * 0.8 and 0.7 * 0.2
                      case AnywhereAnyTime => 0.7 * dryManureVillageForP
                      case OwnerOnly => 0.7 * manureKforAssignedP
                    }
                  case false => 0.0
                }
              case false => 0.0
            }

            val wetManureVillageForP = manureVillageForPFor(parcel, World.fallowParcels(state.world), state)
            val manureKforFallow = kitchen.herdSize * Constants.KG_OF_MANURE_PER_COW_PER_YEAR * parcel.area / World.fallowParcelsForKitchen(state.world, kitchen).map {
              _.area
            }.sum

            val wetMass = parcel.crop match {
              case Fallow =>
                kitchen.wetSeasonHerdStrategy match {
                  case EverywhereByDayOwnerByNight => 0.24 * manureKforFallow + 0.06 * wetManureVillageForP // 0.3 * 0.8 and 0.3 * 0.2
                  case AnywhereAnyTime => 0.3 * wetManureVillageForP
                  case OwnerOnly => 0.3 * manureKforFallow
                }
              case _ => 0.0
            }
            dryMass + wetMass
          }.getOrElse(0.0)
        //FIXME: mulching mass
        val mulchingMass = 0.0
        val currentYearSoilQuality = Fertility.soilQuality(parcel, soilQualityBasis)
        val metrics = Fertility.Metrics(state.year, parcel.crop, manureMass, mulchingMass, AgronomicMetrics(availableNitrogen(parcel, currentYearSoilQuality), currentYearSoilQuality))
        fertilize(
          allParcels.tail,
          fertilityUpdated :+ parcel.copy(fertilityHistory = parcel.fertilityHistory :+ metrics)
        )
      }
    }

    val newParcels = fertilize(state.world.parcels, Seq())

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
